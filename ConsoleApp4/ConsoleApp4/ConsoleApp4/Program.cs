using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using wecr.dwh.metadata.domain.api;
using wecr.dwh.engine.rules;
using System.Reflection;

namespace ConsoleApp4
{
    public static class MyExtensions
    {
        public static int WordCount(this String str)
        {
            return str.Split(new char[] { ' ', '.', '?' },
                             StringSplitOptions.RemoveEmptyEntries).Length;
        }
    }

    class Program
    {
        static void Main(string[] args)
        {
            string server = @"scomp6192\TEST01";
            MetaDataProvider metaDataProvider = new MetaDataProvider(server, 1).IntializeWithModelConfiguration(2);
            var rule = metaDataProvider.Rules.FirstOrDefault(e => e is CalculationRule && ((CalculationRule)e).Property.Name == "TotRevOther" && ((CalculationRule)e).Entity.Name == "IncomeStatementRevenue");
            DataSetDB2 imDatabase = new DataSetDB2();
            var idList = new List<string>() { "1", "2", "3", "28" };
            imDatabase.GetChunk(metaDataProvider.DataMart, metaDataProvider.DataMart.Database.GetConnectionString(), idList, new List<ValidationItem>());

            // Get the content of the private property data.
            Type typ = typeof(DataSetDB);
            FieldInfo type = typ.GetField("data", System.Reflection.BindingFlags.NonPublic | System.Reflection.BindingFlags.Instance);
            var value = type.GetValue(imDatabase);
            var result = typeof(DataSetDB)
                .GetMethod("data", BindingFlags.NonPublic | BindingFlags.Instance)
                .Invoke(imDatabase, new object[0]);

            RuleEngine ruleEngine = new CSharpRuleEngine(metaDataProvider, metaDataProvider.DataMart, false, imDatabase, "00000000-0000-0000-0000-000000000000", metaDataProvider.DataMart.Database.GetConnectionString());
            ruleEngine.ExecuteRules();
        }
    }
}
