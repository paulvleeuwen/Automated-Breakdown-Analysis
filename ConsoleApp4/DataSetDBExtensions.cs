using System;
using System.Collections.Generic;
using System.Data;
using System.Data.SqlClient;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Threading.Tasks;
using wecr.dwh.metadata.domain;
using wecr.dwh.metadata.domain.api;
using Rule = wecr.dwh.metadata.domain.api.Rule;

namespace ConsoleApp4
{
    public static class DataSetDBExtensions
    {

        public static void GetChunk(this DataSetDB imDatabase, Rule rule, Entity rootEntity, string connectionString, List<string> idList)
        {
            // TODO maak van idList een list met BIN nummers.
            var entities = rule.GetEntities(); // <- extensionmethod op rule om        relevante entiteiten op te halen
                                               // get de private data properyvar entities =  rule.GetEntities()  
                                               // vul de data property op een wijze vergleijkbaar met de getchucnk

            SqlDataAdapter adapter = null;
            string queryString = null;
            string tableString = rootEntity.FullyQualifiedName();
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
                    adapter.Fill(imDatabase.GetData(), rootEntity.Name);
                    foreach (Entity entity in entities)
                    {
                        queryString = "SELECT * FROM " + entity.FullyQualifiedName() + " WHERE " + entity.RootRelation.SQLName() + " in " + idsList;
                        adapter = new SqlDataAdapter();
                        adapter.SelectCommand = new SqlCommand(queryString, conn);
                        adapter.SelectCommand.CommandTimeout = 3600;
                        adapter.Fill(imDatabase.GetData(), entity.Name);
                    }
                }
            }
            catch (Exception e)
            {
                //Define error handling
            }
        }

        public static void GetChunk(this DataSetDB imDatabase, Rule rule, Entity rootEntity, string connectionString, List<int> farmId)
        {
            // TODO maak van idList een list met BIN nummers.
            var entities = rule.GetEntities(); // <- extensionmethod op rule om        relevante entiteiten op te halen
                                               // get de private data properyvar entities =  rule.GetEntities()  
                                               // vul de data property op een wijze vergleijkbaar met de getchucnk

            SqlDataAdapter adapter = null;
            string queryString = null;
            string tableString = rootEntity.FullyQualifiedName();
            string farmIdQuery = "(" + String.Join(",", farmId.ToArray()) + ")";
            object[] farmYearId;
            SqlConnection conn = new SqlConnection(connectionString);
            conn.Open();

            queryString = "SELECT * FROM " + tableString + " WHERE IdentifyingEntityID in " + farmIdQuery;
            adapter = new SqlDataAdapter();
            adapter.SelectCommand = new SqlCommand(queryString, conn);
            adapter.SelectCommand.CommandTimeout = 3600;
            adapter.Fill(imDatabase.GetData(), rootEntity.Name);

            //// Get all [WECR_BINSQL_DM].[BINSQL].[AEE].[ID] and provide those as the farmId.
            DataTable dataTableIds = imDatabase.GetData().Tables[rootEntity.Name];
            farmYearId = dataTableIds.AsEnumerable().Select(e => e["ID"]).ToArray();
            string farmYearIdQuery = "(" + String.Join(",", farmYearId) + ")";
            foreach (Entity entity in entities)
            {
                string query = "SELECT\n" +
                    " years.Year AS year\n" +
                    ",main2.*\n" +
                    "FROM\n" +
                    "(\n" +
                    " SELECT\n" +
                    "  main.*\n" +
                    " " + entity.FullyQualifiedName() + " main\n" +
                    " INNER JOIN\n" +
                    " (\n" +
                    "  SELECT\n" +
                    "   ID\n" +
                    "  FROM\n" +
                    "  [WECR_BINSQL_DM].[BINSQL].[AEE]\n" +
                    "  WHERE IdentifyingEntityID in (" +
                    farmIdQuery +
                    ")\n" +
                    "  ) id\n" +
                    " ON main.AEEID = id.ID\n" +
                    ") main2\n" +
                    "LEFT JOIN[WECR_BINSQL_DM].[Dimensions].Years years ON main2.CalendarID = years.ID\n" +
                    "ORDER BY main2.AEE, year";
                query = "SELECT *\n" +
                    "FROM " +
                    entity.FullyQualifiedName() + "\n" +
                    "WHERE [AEEID] in " +
                    farmYearIdQuery +
                    "ORDER BY CalendarID";
                adapter = new SqlDataAdapter();
                adapter.SelectCommand = new SqlCommand(query, conn);
                adapter.SelectCommand.CommandTimeout = 3600;
                adapter.Fill(imDatabase.GetData(), entity.Name);

                // Make sure that the number of rows add up: the length of the farm-year IDs is the length of each time series found, i.e. the number of periods.
                // Since multiple time series are there, the total number of rows is a multiple of the number of periods.
                //if (Math.Round((double)(imDatabase.GetData().Tables[entity.Name].Rows.Count / farmYearId.Length)) != imDatabase.GetData().Tables[entity.Name].Rows.Count / farmYearId.Length)
                //{
                //    throw new AggregateException("The length of the farm-year IDs is the length of each time series found, i.e. the number of periods which is " + farmYearId.Length +
                //        ". Since multiple time series are there, the total number of rows should be a multiple of the number of periods which is not the case as the total number of rows is " +
                //        imDatabase.GetData().Tables[entity.Name].Rows.Count + ".");
                //}
            }
        }

        public static DataSet GetData(this DataSetDB imDatabase)
        {
            FieldInfo field = imDatabase.GetType().GetField("data", System.Reflection.BindingFlags.NonPublic | System.Reflection.BindingFlags.Instance);
            return (DataSet)field.GetValue(imDatabase);
        }

    }
}
