using wecr.dwh.metadata.domain;
using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Data;
using System.Data.SqlClient;
using System.Linq;
using System.Security.Cryptography;
using System.Text;
using System.Threading.Tasks;
using System.Dynamic;

namespace wecr.dwh.metadata.domain.api
{
    public class DataSetDB2 : IInMemoryDB
    {
        public System.Data.DataSet data;
        private ConcurrentDictionary<string, DataRow> currentRows;
        private Entity rootEntity;

        public DataSetDB2()
        {
            this.data = new System.Data.DataSet();
            this.currentRows = new ConcurrentDictionary<string, DataRow>();
            this.data.EnforceConstraints = false;
        }

        public void Reset()
        {
            this.currentRows.Clear();
        }

        public void CreateRecord(Entity entity, ExpandoObject targetObj)
        {
            DataRow newRow = this.data.Tables[entity.Name].NewRow();
            this.data.Tables[entity.Name].Rows.Add(newRow);
            foreach (var prop in targetObj.GetProperties()) newRow[prop.Key] = prop.Value == null ? DBNull.Value : prop.Value;
        }

        public void EnforceConstraints(bool value)
        {
            this.data.EnforceConstraints = value;
        }

        public List<IRecord> GetRecords(string name)
        {
            List<IRecord> records = new List<IRecord>();
            foreach (DataRow row in this.data.Tables[name].Rows)
            {
                records.Add(new DataSetRecord(row));
            }
            return records;
        }

        public List<IRecord> GetChildRecords(IRecord record, string relationName)
        {
            List<IRecord> records = new List<IRecord>() { record };
            List<DataRelation> relations = new List<DataRelation>();
            string tableName = record.GetTableName();
            DataTable rootTable = data.Tables[tableName];
            this.FindRelations(rootTable, relationName, relations);
            List<IRecord> nextRecords = null;
            foreach (DataRelation relation in relations)
            {
                nextRecords = new List<IRecord>();
                foreach (IRecord rec in records)
                {
                    nextRecords.AddRange(rec.GetChildRecords(relation.RelationName));
                }
                records = nextRecords;
            }
            return records;
        }

        private void FindRelations(DataTable rootTable, string relationName, List<DataRelation> relations)
        {
            DataTable relationTable = data.Tables[relationName];
            DataTable parentTable = relationTable.ParentRelations == null ? null : relationTable.ParentRelations[0].ParentTable;
            DataRelation relation = null;
            while (parentTable != null && parentTable != rootTable)
            {
                relation = parentTable.ChildRelations[relationName];
                relations.Insert(0, relation);
                relationTable = data.Tables[parentTable.TableName];
                relationName = parentTable.TableName;
                parentTable = relationTable.ParentRelations == null ? null : relationTable.ParentRelations[0].ParentTable;
            }
            relations.Insert(0, parentTable.ChildRelations[relationName]);
        }

        public void Initialize(List<Entity> entities, Entity rootEntity)
        {
            DataTable tableData = null;
            foreach (Entity entity in entities)
            {
                tableData = new DataTable(entity.Name);
                this.data.Tables.Add(tableData);
                DataColumn column = null;
                foreach (Property prop in entity.Properties)
                {
                    column = new DataColumn(prop.Name, prop.GetDataType());
                    column.AllowDBNull = prop.Nullable;
                    this.data.Tables[entity.Name].Columns.Add(column);
                }
               // this.data.Tables[entity.Name].Columns.Add(new DataColumn("idString", typeof(string)));
            }
            this.rootEntity = rootEntity;
            this.InitializeRelations(rootEntity);
        }

        public void InitializeRelations(Entity entity)
        {
            if (entity.Identifier != null) this.data.Tables[entity.Name].PrimaryKey = new DataColumn[] { this.data.Tables[entity.Name].Columns[entity.Identifier.Name] };
            if (entity.ParentEntity != null)
            {
                DataTable parent = this.data.Tables[entity.ParentEntity.Name];
                this.data.Relations.Add(entity.Name, parent.Columns[entity.ParentEntity.Identifier.Name],
                    this.data.Tables[entity.Name].Columns[entity.ParentRelation.Property.Name]);
            }
            foreach (Entity ent in entity.ChildEntities) this.InitializeRelations(ent);
        }

        private DataRow GetCurrentRow(Entity entity)
        {
            DataRow currentRow = null;
            this.currentRows.TryGetValue(entity.Name, out currentRow);
            if (currentRow == null) return this.data.Tables[entity.Name].Rows[this.data.Tables[entity.Name].Rows.Count-1];
            return currentRow;
        }

        private DataTable GetTable(Entity entity)
        {
            return this.data.Tables[entity.Name];
        }

        public void FlagDataSet(bool valid)
        {
            this.GetCurrentRow(this.rootEntity).SetField("IsValid", valid);
        }

        public void FlagDataSets(IValidationLogger logger)
        {
            foreach (DataRow row in this.data.Tables[this.rootEntity.Name].Rows)
            {
                BitConverter.ToString((byte[])row[this.rootEntity.Identifier.Name]);
                var valid = logger.CheckErrorLog(BitConverter.ToString((byte[])row[this.rootEntity.Identifier.Name]));
                row.SetField("IsValid", valid);
            }
        }


        public Object GetIdentifierValue(Entity entity)
        {
            return this.GetCurrentRow(entity)[entity.Identifier.Name];
        }

        public void AddIdentifier(Entity entity)
        {
            if (entity.EntityType != EntityType.DATASET) return;
            ClassificationElement ce = null;
            DataRow currentRow = this.currentRows[entity.Name];
            foreach (Property prop in entity.Properties.Where(e => e.IsKey()))
            {
                if (currentRow[prop.Name] == null)
                {
                    ce = prop.Classification.ClassificationElements.FirstOrDefault(e => e.InternalID == 0);
                    currentRow.SetField<int>(prop.Name, ce.ID);
                }
            }
        }

        public object GetValueFor(Entity entity, string propertyName)
        {
            return this.GetCurrentRow(entity)[propertyName];
        }

        public void CheckZero(Entity entity)
        {
            if (entity.Properties.Count(e => e.IsData()) == 0) return;
            List<DataRow> rowsToDelete = new List<DataRow>();
            foreach (DataRow row in data.Tables[entity.Name].Rows)
            {
                if (entity.Properties.Where(e =>
                    e.IsData() && e.IsNumeric() && row[e.Name] != DBNull.Value && Decimal.Compare(Convert.ToDecimal(row[e.Name]), 0) != 0).Count() == 0)
                        rowsToDelete.Add(row);
            }
            rowsToDelete.ForEach(e => data.Tables[entity.Name].Rows.Remove(e));
        }

        public Type GetTypeFor(Entity entity, string columnName)
        {
            return this.GetTable(entity).Columns[columnName].DataType;
        }

        public void CheckConstraints(Entity rootEntity, List<Entity> entities, List<ValidationItem> errors)
        {
            this.CheckSign(entities, errors);
            try
            {
                this.data.EnforceConstraints = true;
            }
            catch (ConstraintException)
            {
                Entity entityMapping = null;
                foreach (DataTable tab in this.data.Tables)
                {
                    entityMapping = entities.SingleOrDefault(e => e.Name == tab.TableName);
                    foreach (DataRow row in tab.GetErrors())
                    {
                        foreach (DataColumn col in row.GetColumnsInError())
                        {
                            row[col.ColumnName] = Activator.CreateInstance(col.DataType);
                            errors.Add(new ValidationItem(entityMapping.Name + "." + col.ColumnName, "Field cannot be empty", 1, BitConverter.ToString((byte[])this.GetValueFor(rootEntity, "ID"))));
                        }
                    }
                }
            }
            finally
            {
                this.data.EnforceConstraints = false;
            }

        }

        protected void CheckSign(List<Entity> entities, List<ValidationItem> errors)
        {
            IEnumerable<Property> unsignedColumns = null;
            object value = null;
            bool isNegative = false;
            foreach (Entity entity in entities)
            {
                unsignedColumns = entity.Properties.Where(e => e.IsNumeric() && !e.Signed);
                foreach (DataRow row in this.GetTable(entity).Rows)
                {
                    foreach (Property prop in unsignedColumns)
                    {
                        value = row[prop.Name];
                        if (value == DBNull.Value) continue;
                        switch (Type.GetTypeCode(value.GetType()))
                        {
                            case TypeCode.Int32:
                                isNegative = (int)value < 0;
                                break;
                            case TypeCode.Int64:
                                isNegative = (long)value < 0;
                                break;
                            case TypeCode.Decimal:
                                isNegative = (decimal)value < 0;
                                break;
                            case TypeCode.Double:
                                isNegative = (double)value < 0;
                                break;
                            default:
                                break;
                        }
                        if (isNegative) errors.Add(new ValidationItem(entity.Name + "." + prop.Name, "Negative Values Not Allowed", 1, BitConverter.ToString((byte[])this.GetValueFor(rootEntity, "ID"))));
                    }
                }
            }
        }

        public List<string> GetChunkIDs(string connectionString, string batchIdentifier, ModelImplementation datawarehouse)
        {
            List<List<string>> idLists = new List<List<string>>();
            StringBuilder queryBuilder = new StringBuilder();
            queryBuilder.Append("SELECT DISTINCT ");
            queryBuilder.Append(datawarehouse.HistoryEntity.RootRelation.SQLName());
            queryBuilder.Append(" FROM ");
            queryBuilder.Append(datawarehouse.HistoryEntity.FullyQualifiedName());
            if (batchIdentifier != null && batchIdentifier != "00000000-0000-0000-0000-000000000000")
            {
                queryBuilder.Append(" WHERE BatchIdentifier = '");
                queryBuilder.Append(batchIdentifier);
                queryBuilder.Append("'");
            }
            List<string> idList = new List<string>();
            using (SqlConnection connection = new SqlConnection(connectionString))
            {
                using (SqlCommand command = new SqlCommand(queryBuilder.ToString(), connection))
                {
                    command.CommandTimeout = 3600;
                    using (SqlDataAdapter adapter = new SqlDataAdapter())
                    {
                        adapter.SelectCommand = command;
                        using (DataTable idData = new DataTable())
                        {
                            adapter.Fill(idData);
                            foreach (DataRow row in idData.Rows) idList.Add(((int)row[datawarehouse.HistoryEntity.RootRelation.Property.Name]).ToString());
                        }
                    }
                }
            }
            return idList;
        }
        public List<List<string>> GetChunkIDs(string connectionString, int flushSize, string batchIdentifier, ModelImplementation datawarehouse)
        {
            List<List<string>> idLists = new List<List<string>>();
            StringBuilder queryBuilder = new StringBuilder();
            queryBuilder.Append("SELECT DISTINCT ");
            queryBuilder.Append(datawarehouse.HistoryEntity.RootRelation.SQLName());
            queryBuilder.Append(" FROM ");
            queryBuilder.Append(datawarehouse.HistoryEntity.FullyQualifiedName());
            if (batchIdentifier != null && batchIdentifier != "00000000-0000-0000-0000-000000000000")
            {
                queryBuilder.Append(" WHERE BatchIdentifier = '");
                queryBuilder.Append(batchIdentifier);
                queryBuilder.Append("'");
            }
            List<string> idList = new List<string>();
            using (SqlConnection connection = new SqlConnection(connectionString))
            {
                using (SqlCommand command = new SqlCommand(queryBuilder.ToString(), connection))
                {
                    command.CommandTimeout = 3600;
                    using (SqlDataAdapter adapter = new SqlDataAdapter())
                    {
                        adapter.SelectCommand = command;
                        using (DataTable idData = new DataTable())
                        {
                            adapter.Fill(idData);
                            foreach (DataRow row in idData.Rows) idList.Add(((int)row[datawarehouse.HistoryEntity.RootRelation.Property.Name]).ToString());
                        }
                    }
                }
            }
            idLists = new List<List<string>>();
            for (int i = 0; i < idList.Count; i += flushSize)
            {
                idLists.Add(idList.GetRange(i, Math.Min(flushSize, idList.Count - i)));
            }
            return idLists;
        }

        public void GetChunk(ModelImplementation implementation, string connectionString, List<string> idList, List<ValidationItem> errors)
        {
            SqlDataAdapter adapter = null;
            string queryString = null;
            string tableString = implementation.RootEntity.FullyQualifiedName();
            string idsList = "(" + String.Join(",", idList.ToArray()) + ")";
            try
            {
                using (SqlConnection conn = new SqlConnection(connectionString))
                {
                    conn.Open();
                    queryString = "SELECT * FROM " + tableString + " WHERE ID in " + idsList;
                    adapter = new SqlDataAdapter();
                    adapter.SelectCommand = new SqlCommand(queryString, conn);
                    adapter.SelectCommand.CommandTimeout = 3600;
                    adapter.Fill(this.data, implementation.RootEntity.Name);
                    foreach (Entity entity in implementation.FactEntities)
                    {
                        queryString = "SELECT * FROM " + entity.FullyQualifiedName() + " WHERE " + entity.RootRelation.SQLName() + " in " + idsList;
                        adapter = new SqlDataAdapter();
                        adapter.SelectCommand = new SqlCommand(queryString, conn);
                        adapter.SelectCommand.CommandTimeout = 3600;
                        adapter.Fill(this.data, entity.Name);
                    }
                }
            }
            catch (Exception e)
            {
                errors.Add(new ValidationItem("Failed to get dataset chunks", "DataBase Error: " + e.Message, 1, "General"));
            }
        }

        public void Clear()
        {
            this.data.Clear();
        }

        private void CreateDeleteStatement(Entity entity, StringBuilder builder, string idsList)
        {
            foreach (Entity child in entity.ChildEntities) this.CreateDeleteStatement(child, builder, idsList);
            if (entity.HasRules)
            {
                builder.Append("DELETE FROM ");
                builder.Append(entity.FullyQualifiedName());
                builder.Append(" WHERE ");
                builder.Append(entity.EntityType == EntityType.DATASET ? entity.Identifier.SQLName() : entity.RootRelation.SQLName());
                builder.Append(" in ");
                builder.Append(idsList);
                builder.AppendLine(";");
            }
        }

        public void WriteChunk(ModelImplementation implementation, string connectionString, List<string> idList, List<ValidationItem> errors)
        {
            StringBuilder commBuilder = new StringBuilder();
            string idsList = "(" + String.Join(",", idList.ToArray()) + ")";
            this.CreateDeleteStatement(implementation.RootEntity, commBuilder, idsList);
            string query = commBuilder.ToString();
            if (String.IsNullOrEmpty(query)) return;
            try
            {
                using (SqlConnection conn = new SqlConnection(connectionString))
                {
                    conn.Open();
                    SqlCommand cmd = new SqlCommand(commBuilder.ToString(), conn);
                    cmd.CommandTimeout = 3600;
                    cmd.ExecuteNonQuery();
                }
            }
            catch (Exception e)
            {
                errors.Add(new ValidationItem("Failed to delete existing datamart records", "DataBase Error: " + e.Message, 1, "General"));
            }
            this.WriteAllData(implementation.Database, implementation.RootEntity, implementation.Entities, errors);
        }

        public void WriteAllData(DataBase database, Entity rootEntity, List<Entity> entities, List<ValidationItem> errors)
        {
            string targetConnection = database.GetConnectionString();
            if (targetConnection == null) return;
            DataTable dbTable = null;
            using (SqlConnection connection = new SqlConnection(targetConnection))
            {
                try
                {
                    connection.Open();
                }
                catch(Exception)
                {
                    errors.Add(new ValidationItem(connection.Database, "DataBase Error: cannot acquire connection. Try increasing record flush count", 1, (string)this.GetValueFor(rootEntity, "IdentifyingEntityID")));
                    throw;
                }
                foreach (Entity entity in entities.Where(e => database.DataBaseType == DataBaseType.STAGING || e.HasRules))
                {
                    using (SqlBulkCopy sqlBulkCopy = new SqlBulkCopy(targetConnection, SqlBulkCopyOptions.TableLock))
                    {
                        try
                        {
                            sqlBulkCopy.BulkCopyTimeout = 3600;
                            sqlBulkCopy.DestinationTableName = entity.FullyQualifiedName(); ;
                            dbTable = this.data.Tables[entity.Name];
                            foreach (var column in dbTable.Columns)
                                sqlBulkCopy.ColumnMappings.Add(column.ToString(), column.ToString());
                            sqlBulkCopy.WriteToServer(dbTable);
                        }
                        catch (Exception e)
                        {
                            errors.Add(new ValidationItem(connection.Database + "-" + entity.Name, "DataBase Error: " + e.Message, 1, (string)this.GetValueFor(rootEntity, "IdentifyingEntityID")));
                        }
                    }
                }
            }
        }

        private string GetConnectionName(string serverName)
        {
            StringBuilder csBuilder = new StringBuilder();
            csBuilder.Append("Data Source=");
            csBuilder.Append(serverName);
            csBuilder.Append("; Initial Catalog =WECR_METADATA; Integrated Security = SSPI;Connection Timeout=60");
            return csBuilder.ToString();
        }
        public ClassificationElement AddLateArrivingElement(string value, Classification classification, string serverName, IValidationLogger logger)
        {
            string connectionString = this.GetConnectionName(serverName);
            ClassificationElement ce = null;
            try
            {
                using (SqlConnection conn = new SqlConnection(connectionString))
                {
                    conn.Open();
                    SqlCommand cmd = new SqlCommand("INSERT INTO [WECR_METADATA].[Classification].[CommonConcept]([DateCreated],[DateUpdated],[CreatedByID],[UpdatedByID],[Name],[Description],[DefaultContextID]) VALUES(GETDATE(), GETDATE(),1,1,@name,@name,1); SELECT SCOPE_IDENTITY();", conn);
                    cmd.Parameters.AddWithValue("@name", value);
                    int ccid = (int)(decimal)cmd.ExecuteScalar();
                    cmd = new SqlCommand("INSERT INTO[WECR_METADATA].[Classification].[ClassificationElement]([ClassificationID], [CommonConceptID], [ExternalID], [Power],[Order],[DateCreated], [DateUpdated],[CreatedByID],[UpdatedByID]) VALUES(@cid, @ccid, @ext, 0,0, GETDATE(), GETDATE(),1,1); SELECT SCOPE_IDENTITY();", conn);
                    cmd.Parameters.AddWithValue("@cid", classification.ID);
                    cmd.Parameters.AddWithValue("@ccid", ccid);
                    cmd.Parameters.AddWithValue("@ext", value);
                    ce = new ClassificationElement((int)(decimal)cmd.ExecuteScalar(), value);
                    ce.ExternalID = value;
                    ce.InternalID = ccid;
                    cmd = new SqlCommand(@"INSERT INTO[WECR_DWH].[General].[Classification](MaxLevel_Internal_ID, MaxLevel_Internal_Name, MaxLevel_External_ID, MaxLevel_Element_ID, Classificationname, ClassificationID, ClassificationType, Begin_Date, End_Date, Path_Length, Level_01_Internal_ID, Level_01_Internal_Name, Level_01_External_ID, Level_01_Element_ID)
                                           VALUES(@inid, @inna, @exid, @elid, @clna, @clid, @ctid, '1899-01-01 00:00:00.0000000', '9999-12-31 00:00:00.0000000', 1, @inid,@inna, @exid,@elid)", conn);
                    cmd.Parameters.AddWithValue("@inid", ce.InternalID);
                    cmd.Parameters.AddWithValue("@inna", ce.Name);
                    cmd.Parameters.AddWithValue("@exid", ce.ExternalID);
                    cmd.Parameters.AddWithValue("@elid", ce.ID);
                    cmd.Parameters.AddWithValue("@clna", classification.Name);
                    cmd.Parameters.AddWithValue("@clid", classification.ID);
                    cmd.Parameters.AddWithValue("@ctid", 5);
                    cmd.ExecuteNonQuery();
                }
            }
            catch (Exception e)
            {
                logger.NewLog("Database", "Classifications", "All");
                logger.AddToLog("Database", 1, e.Message, "LateArriving");
            }
            return ce;
         
        }

        public void AddTuple(ClassificationElement element, List<ClassificationElement> tuple, string serverName, IValidationLogger logger)
        {
            StringBuilder queryBuilder = new StringBuilder();
            for(int i = 0;  i < tuple.Count; i++)
            {
                queryBuilder.Append("INSERT INTO [WECR_METADATA].[Classification].[CommonConceptTuple]([CommonConceptID],[ParentCommonConceptID],[Index]) VALUES(");
                queryBuilder.Append(element.InternalID.ToString());
                queryBuilder.Append(",");
                queryBuilder.Append(tuple[i].InternalID.ToString());
                queryBuilder.Append(",");
                queryBuilder.Append(i.ToString());
                queryBuilder.AppendLine(");");
            }
            string connectionString = this.GetConnectionName(serverName);
            try
            {
                using (SqlConnection conn = new SqlConnection(connectionString))
                {
                    conn.Open();
                    SqlCommand cmd = new SqlCommand(queryBuilder.ToString(), conn);
                    cmd.ExecuteNonQuery();
                }
            }
            catch (Exception e)
            {
                logger.NewLog("Database", "Classifications", "All");
                logger.AddToLog("Database", 1, e.Message, "LateArriving");
            }
        }


    }
}
