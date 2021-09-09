using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using wecr.dwh.metadata.domain.api;
using wecr.dwh.engine.rules;
using System.Reflection;
using System.Data;
using Rule = wecr.dwh.metadata.domain.api.Rule;
using NReco.PivotData;

namespace ConsoleApp4
{
    public static class MyExtensions
    {
        public static int WordCount(this String str)
        {
            return str.Split(new char[] { ' ', '.', '?' },
                             StringSplitOptions.RemoveEmptyEntries).Length;
        }

        /// <summary>
        /// Gets a Inverted DataTable. Taken from https://www.codeproject.com/Articles/22008/C-Pivot-Table.
        /// </summary>
        /// <param name="table">Provided DataTable</param>
        /// <param name="columnX">X Axis Column</param>
        /// <param name="columnY">Y Axis Column</param>
        /// <param name="columnsZ">Z Axis Column (values)</param>
        /// <param name="columnsToIgnore">Whether to ignore some column, it must be 
        /// provided here</param>
        /// <param name="nullValue">null Values to be filled</param> 
        /// <returns>DataTable with pivoted values</returns>
        public static DataTable GetInversedDataTable(DataTable table, string columnX, string columnY, string[] columnsZ)
        {
            // Make sure that all colum names are present in table.
            if (!table.Columns.Contains(columnX))
            {
                throw new ArgumentException("The input argument columnX is expected to be part of the column names of the input argument table.");
            }
            if (!table.Columns.Contains(columnY))
            {
                throw new ArgumentException("The input argument columnY is expected to be part of the column names of the input argument table.");
            }
            foreach(string colName in columnsZ)
            {
                if (!table.Columns.Contains(colName))
                {
                    throw new ArgumentException("Every element of the input argument columnsZ is expected to be part of the column names of the input argument table.");
                }
            }            

            // Make sure that all X-columns yield unique combinations of values.
            var queryCountsPerXValues = table.AsEnumerable()
                .GroupBy(r => r.Field<object>(columnX))
                .Select(grp => new
                {
                    value = grp.Key,
                    Count = grp.Count()
                })
                .OrderBy(o => o.value)
                .ToList();
            int nDistinctYValues = table.AsEnumerable().Select(e => e[columnY]).Distinct().Count();
            bool isUnique = queryCountsPerXValues.Select(e => e.Count).All(e => e.Equals(nDistinctYValues));
            if (!isUnique)
            {
                throw new Exception("The number of distinct y-values (" +
                    nDistinctYValues +
                    ") times the number of columns to be added ("+
                    queryCountsPerXValues.Count +
                    "), which is " +
                    nDistinctYValues * queryCountsPerXValues.Count +
                    ", is expected to be equal to the number of rows (" +
                    table.Rows.Count +
                    ").");
            }

            //Create a DataTable to return.
            DataTable returnTable = new DataTable();

            //Add a Column at the beginning of the table
            returnTable.Columns.Add(columnY, typeof(decimal));

            //Read all DISTINCT values from columnX Column in the provided DataTale
            List<string> columnXValues = new List<string>();

            foreach (DataRow dr in table.Rows)
            {
                foreach (string columnZ in columnsZ)
                {
                    string columnXTemp = columnZ + " (" + columnX + " = " + dr[columnX].ToString() + ")";
                    //Read each row value, if it's different from others provided, add to 
                    //the list of values and creates a new Column with its value.
                    if (!columnXValues.Contains(columnXTemp))
                    {
                        columnXValues.Add(columnXTemp);
                        returnTable.Columns.Add(columnXTemp);
                    }
                }
            }
            
            //Read DISTINCT Values for Y Axis Column
            object[] columnYValuesCheck = table.AsEnumerable().Select(e => e[columnY]).Distinct().ToArray();
            List<object> columnYValues = new List<object>();
            foreach (DataRow dr in table.Rows)
            {
                if (!columnYValues.Contains(dr[columnY]))
                    columnYValues.Add(dr[columnY]);
            }

            // Iterate over all distinct y values.
            foreach (object columnYValue in columnYValues)
            {
                //Creates a new Row
                DataRow drReturn = returnTable.NewRow();
                drReturn[0] = columnYValue;
                //foreach column Y value, The rows are selected distincted
                DataRow[] rows = table.Select(columnY + "='" + columnYValue + "'");

                //Read each row to fill the DataTable
                foreach (DataRow dr in rows)
                {
                    foreach (string columnZ in columnsZ)
                    {
                        string rowColumnTitle = columnZ + " (" + columnX + " = " + dr[columnX].ToString() + ")";

                        //Read each column to fill the DataTable
                        foreach (DataColumn dc in returnTable.Columns)
                        {
                            if (dc.ColumnName == rowColumnTitle)
                            {
                                // Vraag aan EW: waarom wordt drReturn[rowColumnTitle] een string terwijl dr[columnZ] dat niet is?
                                // En kunnen we dit niet voor elke kolom in een keer doen? Moeten we over alle rijen itereren?
                                drReturn[rowColumnTitle] = dr[columnZ];
                            }
                        }
                    }
                }
                returnTable.Rows.Add(drReturn);
            } // iterate over all values of column Y
            return returnTable;
        } // function GetInversedDataTable
    }


    class Program
    {
        static void Main(string[] args)
        {

            // Main settings.
            string server = @"scomp6240";
            List<int> farmIds = new List<int>() { 41277 };
            string tableName = "IncomeStatementRevenue";
            string varNameAnalysis = "TotRevOther";

            MetaDataProvider metaDataProvider = new MetaDataProvider(server, 1).IntializeWithModelConfiguration(2);
            var rule = metaDataProvider.Rules.FirstOrDefault(e => e is CalculationRule && ((CalculationRule)e).Property.Name == varNameAnalysis && ((CalculationRule)e).Entity.Name == tableName);
            DataSetDB imDatabase = new DataSetDB();
            imDatabase.Initialize(metaDataProvider.DataMart.Entities, metaDataProvider.DataMart.RootEntity);
            imDatabase.GetChunk(rule, metaDataProvider.DataMart.RootEntity, metaDataProvider.DataMart.Database.GetConnectionString(), farmIds);

            // Iterate over all column names to mutate. For each column name, replace the current value with the value of previous year.
            // Then execute the rule in question.

            // Set up the rule engine.
            RuleEngine ruleEngine = new CSharpRuleEngine(
                metaDataProvider,
                metaDataProvider.DataMart,
                false,
                imDatabase,
                "00000000-0000-0000-0000-000000000000",
                metaDataProvider.DataMart.Database.GetConnectionString());

            // Vraag aan EW: wat moet ik met allRules? En welke rule / variabele wordt niet door een andere gebruikt?
            var allRules = ruleEngine.CalculationRules;

            ruleEngine.CalculationRules = new List<Rule>() { rule };

            // Iterate over all column names to mutate.
            DataTable dataset = imDatabase.GetData().Tables[tableName];
            //dataset.Rows[12]["TotRev"] = dataset.Rows[12]["TotRev"];
            //ruleEngine.ExecuteRules();

            DataTable datasetCopy = dataset.Copy();
            string[] colNamesMutate = rule.ExpressionNodeList.Where(e => e is wecr.dwh.metadata.domain.expressions.variables.PropertyNode).Select(e => e.Property.Name).ToArray();
            string[] colNamesDimensionOrBinary = metaDataProvider.DataMart.Entities.FirstOrDefault(e => e.Name == tableName).Properties.Where(e => e.PropertyType == PropertyType.DIMENSION || e.IsSubKey()).Select(e => e.Name).ToArray();
            foreach (string colName in colNamesMutate)
            {
                int iRow2 = 0;
                DataRow test = dataset.Rows[12];
                foreach (DataRow currentDataRow in dataset.Rows)
                {
                    DataRow previousRecord = datasetCopy.AsEnumerable().ToList().FirstOrDefault(
                        e => (int)e["CalendarID"] == (int)currentDataRow["CalendarID"] - 1 && 
                        (bool)e[colNamesDimensionOrBinary[0]] == (bool)currentDataRow[colNamesDimensionOrBinary[0]]);
                    if (previousRecord != null)
                    {
                        if(iRow2 == 12)
                        {
                            iRow2 = iRow2;
                        }
                        currentDataRow[colName] = previousRecord[colName];
                    }
                    iRow2++;
                    ruleEngine.ExecuteRules();
                }
                ruleEngine.ExecuteRules();

                // Determine the impact of the change in the current variable with respect to the variable under study.
                dataset.Columns.Add("impactNotNormalised", typeof(double));
                for (int iRow = 1; iRow < dataset.Rows.Count; iRow++) // omit the first row as the change can only be calculated from the second period on
                {
                    double yPrevious = Convert.ToDouble(datasetCopy.Rows[iRow - 1][varNameAnalysis]);
                    double yCurrent = Convert.ToDouble(datasetCopy.Rows[iRow][varNameAnalysis]);
                    if (yCurrent == yPrevious || dataset.Rows[iRow][varNameAnalysis].ToString() == "")
                    {
                        dataset.Rows[iRow]["impactNotNormalised"] = 0;
                    }
                    else
                    {
                        double yCurrentMutated = Convert.ToDouble(dataset.Rows[iRow][varNameAnalysis]);
                        dataset.Rows[iRow]["impactNotNormalised"] = (yCurrent - yCurrentMutated) / (yCurrent - yPrevious);
                    }                    
                }

                // Reset the dataset to the original one enabling the analysis of the next variable.
                dataset = datasetCopy;
            } // iterate over all variables to mutate          
        }
    }
}
