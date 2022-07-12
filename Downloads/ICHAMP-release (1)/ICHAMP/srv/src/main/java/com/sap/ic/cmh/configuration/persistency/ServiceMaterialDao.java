package com.sap.ic.cmh.configuration.persistency;

import com.sap.cds.Result;
import com.sap.cds.ql.Select;
import com.sap.cds.services.persistence.PersistenceService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

import cds.gen.configurationservice.ServiceMaterials;
import cds.gen.configurationservice.ServiceMaterials_;

@Repository
public class ServiceMaterialDao {
    
    @Autowired
    PersistenceService db;

    /**To perform select query on ServiceMaterials
	*
	*@param {@link ServiceMaterials} serviceMaterials
	*
	* @public
	*/
    public Result getServiceMaterials() {
        return db
                .run(Select.from(ServiceMaterials_.class).orderBy(c -> c.get("identifier").desc()));
    }

    /**To perform select query on ServiceMaterials based on Destination and Sub Item Type
	*
	*@param {@link ServiceMaterials} serviceMaterials
	*
	* @public
    */
    public Result getServiceMaterialsBasedOnDestinationAndSubItemType(String destination, String subItemType){
        return db
                .run(Select.from(ServiceMaterials_.class).where(b -> b.destination()
                .eq(destination).and(b.subItemType_code().eq(subItemType))));
    }

    /**To perform select query on ServiceMaterials based on Destination, Item Type and Sub Item Type
	*
	*@param {@link ServiceMaterials} serviceMaterials
	*
	* @public
    */
    public Result getServiceMaterialsBasedOnDestinationSubItemTypeAndItemType(String destination, String subItemType, String itemType){
        return db
                .run(Select.from(ServiceMaterials_.class).columns(ServiceMaterials.SERVICE_MATERIAL)
                .where(b -> b.destination()
                .eq(destination).and(b.subItemType_code().eq(subItemType)).and(b.itemType_code().eq(itemType))));
    }

    /**To perform select query on ServiceMaterials based on Sub Item Type
	*
	*@param {@link ServiceMaterials} serviceMaterials
	*
	* @public
    */
    public Result getServiceMaterialsBasedOnSubItemType(String subItemType){
        return db
                .run(Select.from(ServiceMaterials_.class).where(b->b.subItemType_code().eq(subItemType)));
    }


    public Result getServiceMaterialsDetail(String serviceMaterialsID){
        return db.run(Select.from(ServiceMaterials_.class).where(b -> b.get(ServiceMaterials.ID).eq(serviceMaterialsID)));
    }

}