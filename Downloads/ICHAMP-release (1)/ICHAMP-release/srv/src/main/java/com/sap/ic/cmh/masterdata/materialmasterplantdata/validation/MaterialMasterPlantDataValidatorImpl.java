package com.sap.ic.cmh.masterdata.materialmasterplantdata.validation;

import cds.gen.masterdataservice.MaterialMasterPlantDatas;
import cds.gen.masterdataservice.MaterialMasterPlantDatas_;
import com.sap.ic.cmh.gen.MessageKeys;
import com.sap.ic.cmh.utils.Constants;
import com.sap.ic.cmh.utils.LoggerHelper;
import com.sap.ic.cmh.utils.datavalidation.DataValidator;
import org.slf4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class MaterialMasterPlantDataValidatorImpl implements MaterialMasterPlantDataValidator {
    public static final Logger logger = LoggerHelper.getLogger(MaterialMasterPlantDataValidatorImpl.class);

    @Autowired
    private DataValidator dataValidator;

    /**
     * Method used to validate and sanitize the Material master plant detail
     * @param plantData
     */
    @Override
    public void checkInputsSanitized(MaterialMasterPlantDatas plantData) {
        LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "checkInputsSanitized");

        /* Material Code */
        dataValidator.validateData(plantData.getMaterialCode(),
                MessageKeys.MATERIAL_CODE_VALIDATION_ERROR, MaterialMasterPlantDatas_.class, MaterialMasterPlantDatas_::materialCode, true);

        /* Plant */
        dataValidator.validateData(plantData.getPlant(),
                MessageKeys.PLANT_VALIDATION_ERROR, MaterialMasterPlantDatas_.class, MaterialMasterPlantDatas_::plant);

        /* Container Requirements */
        dataValidator.validateData(plantData.getContainerRequirements(),
                MessageKeys.CONTAINER_REQUIREMENTS_VALIDATION_ERROR, MaterialMasterPlantDatas_.class, MaterialMasterPlantDatas_::containerRequirements);

        /* Weight Unit */
        dataValidator.validateDataWithSpecialChar(plantData.getWeightUnitCode(),
                MessageKeys.WEIGHT_UNIT_VALIDATION_ERROR, MaterialMasterPlantDatas_.class, MaterialMasterPlantDatas_::weightUnit_code,false,true,Constants.WTVOLUNITACCEPCHAR);
                
        /* Serial Number Profile */
        dataValidator.validateData(plantData.getSerialNumberProfile(),
                MessageKeys.SERIAL_NUMBER_PROFILE_VALIDATION_ERROR, MaterialMasterPlantDatas_.class, MaterialMasterPlantDatas_::serialNumberProfile);

        /* Reorder Point */
        dataValidator.validateNumber(plantData.getReorderPoint(),
                MessageKeys.REORDER_POINT_VALIDATION_ERROR, MaterialMasterPlantDatas_.class, MaterialMasterPlantDatas_::reorderPoint);

        /* Mrp Type */
        dataValidator.validateData(plantData.getMrpType(),
                MessageKeys.MRP_TYPE_VALIDATION_ERROR, MaterialMasterPlantDatas_.class, MaterialMasterPlantDatas_::mrpType);

        /* Pricing Indicator */
        dataValidator.validateAlphaNumericData(plantData.getPricingIndicator(),
                MessageKeys.PRICE_INDICATOR_VALIDATION_ERROR, MaterialMasterPlantDatas_.class, MaterialMasterPlantDatas_::pricingIndicator);

        LoggerHelper.logMethodExit(logger, this.getClass().getSimpleName(), "checkInputsSanitized");
    }
}