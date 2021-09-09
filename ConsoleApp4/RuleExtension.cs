using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using wecr.dwh.metadata.domain.api;
using wecr.dwh.metadata.domain.expressions.variables;

namespace ConsoleApp4
{
    public static class RuleExtension
    {
        public static IEnumerable<Entity> GetEntities(this  Rule rule)
        {
            var referencedEntities =  rule.ExpressionNodeList.Where(e => e is PropertyNode).Select(e => e.Property.Entity).Distinct();
            var requiredEntities = new List<Entity>();
            foreach(Entity ent in referencedEntities)
            {
                requiredEntities.AddRange(ent.GetWithParents());
            }
            return requiredEntities.Distinct();
        }

    }
}
