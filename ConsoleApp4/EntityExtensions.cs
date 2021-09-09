using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using wecr.dwh.metadata.domain.api;

namespace ConsoleApp4
{
    public static class EntityExtensions
    {

        public static List<Entity> GetWithParents(this Entity entity)
        {
            var parents = new List<Entity>();
            entity.GetParent(parents);
            return parents;
        }
        public static void GetParent(this Entity entity, List<Entity> parents)
        {
            if (entity.ParentEntity == null) return;
            parents.Add(entity);
            entity.ParentEntity.GetParent(parents);
        }
    }
}
