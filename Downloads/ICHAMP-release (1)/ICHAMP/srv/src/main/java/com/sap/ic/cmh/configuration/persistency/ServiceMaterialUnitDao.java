package com.sap.ic.cmh.configuration.persistency;

import com.sap.cds.Result;
import com.sap.cds.ql.Select;
import com.sap.cds.services.persistence.PersistenceService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;
import cds.gen.configurationservice.ServiceMaterialUnits_;

@Repository
public class ServiceMaterialUnitDao {
    
    @Autowired
    PersistenceService db;

    /**To perform select query on ServiceMateriaUnits
	*
	*@param {@link ServiceMaterials} serviceMaterialUnits
	*
	* @public
	*/
    public Result getServiceMaterialUnits() {
        return db
                .run(Select.from(ServiceMaterialUnits_.class).orderBy(c -> c.get("identifier").desc()));
    }

    /**To perform select query on ServiceMateriaUnits
	*
	*@param {@link ServiceMaterials} serviceMaterialUnits
	*
	* @public
	*/
    public Result getServiceMaterialUnitsBasedOnServiceMaterial(String serviceMaterial) {
        return db
                .run(Select.from(ServiceMaterialUnits_.class).where(b -> b.serviceMaterial_ID()
                .eq(serviceMaterial)));
    }

    /**To perform select query on ServiceMateriaUnits based on Service Material and Unit Code
	*
	*@param {@link ServiceMaterials} serviceMaterialUnits
	*
	* @public
	*/
    public Result getServiceMaterialUnitBasedOnMaterialIdAndUnit(String serviceMaterialId, String unitCode) {
        return db
                .run(Select.from(ServiceMaterialUnits_.class).where(b -> b.serviceMaterial_ID()
                .eq(serviceMaterialId).and(b.unit_code().eq(unitCode))));
    }
}
