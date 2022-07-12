package com.sap.ic.cmh.masterdata.materialmastergeneraldata.validation;

import cds.gen.masterdataservice.MaterialMasterGeneralDatas;
import cds.gen.masterdataservice.MaterialMasterGeneralDatas_;
import com.sap.ic.cmh.gen.MessageKeys;
import com.sap.ic.cmh.utils.Constants;
import com.sap.ic.cmh.utils.LoggerHelper;
import com.sap.ic.cmh.utils.datavalidation.DataValidator;
import org.slf4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class MaterialMasterGeneralDataValidatorImpl implements MaterialMasterGeneralDataValidator {

    public static final Logger logger = LoggerHelper.getLogger(MaterialMasterGeneralDataValidatorImpl.class);

    @Autowired
    private DataValidator dataValidator;

    /**
     * Method used to validate and sanitize given master material data
     *
     * @param generalData
     */
    @Override
    public void checkInputsSanitized(MaterialMasterGeneralDatas generalData) {
        LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "checkInputsSanitized");

        /* Material Code */
        dataValidator.validateData(generalData.getMaterialCode(),
                MessageKeys.MATERIAL_CODE_VALIDATION_ERROR, MaterialMasterGeneralDatas_.class, MaterialMasterGeneralDatas_::materialCode, true);

        /* Material Description */
        dataValidator.validateData(generalData.getMaterialDescription(),
                MessageKeys.MATERIAL_DESCRIPTION_VALIDATION_ERROR, MaterialMasterGeneralDatas_.class, MaterialMasterGeneralDatas_::materialDescription);

        /* Material Type */
        dataValidator.validateData(generalData.getMaterialType(),
                MessageKeys.MATERIAL_TYPE_VALIDATION_ERROR, MaterialMasterGeneralDatas_.class, MaterialMasterGeneralDatas_::materialType);
        
        /* Material Group */
        dataValidator.validateData(generalData.getMaterialGroup(),
                MessageKeys.MATERIAL_GROUP_VALIDATION_ERROR, MaterialMasterGeneralDatas_.class, MaterialMasterGeneralDatas_::materialGroup);

        /* Base Unit Of Measure */
        dataValidator.validateDataWithSpecialChar(generalData.getBaseUnitOfMeasureCode(),
                MessageKeys.BASE_UNIT_OF_MEASURE_VALIDATION_ERROR, MaterialMasterGeneralDatas_.class, MaterialMasterGeneralDatas_::baseUnitOfMeasure_code,false,true,Constants.BASEUOMACCEPCHAR);

        /* Division */
        dataValidator.validateData(generalData.getDivision(),
                MessageKeys.DIVISION_VALIDATION_ERROR, MaterialMasterGeneralDatas_.class, MaterialMasterGeneralDatas_::division);

        /* Plant Material Status */
        dataValidator.validateData(generalData.getXPlantMaterialStatus(),
                MessageKeys.X_PLANT_MATERIAL_STATUS_VALIDATION_ERROR, MaterialMasterGeneralDatas_.class, MaterialMasterGeneralDatas_::xPlantMaterialStatus);

        /* Weight Unit */
        dataValidator.validateDataWithSpecialChar(generalData.getWeightUnitCode(),
                MessageKeys.WEIGHT_UNIT_VALIDATION_ERROR, MaterialMasterGeneralDatas_.class, MaterialMasterGeneralDatas_::weightUnit_code,false,true,Constants.WTVOLUNITACCEPCHAR);

        /* Volume Unit */
        dataValidator.validateDataWithSpecialChar(generalData.getVolumeUnitCode(),
                MessageKeys.VOLUME_UNIT_VALIDATION_ERROR, MaterialMasterGeneralDatas_.class, MaterialMasterGeneralDatas_::volumeUnit_code,false,true,Constants.WTVOLUNITACCEPCHAR);

        /* Size Dimensions */
        dataValidator.validateData(generalData.getSizeDimensions(),
                MessageKeys.SIZE_DIMENSIONS_VALIDATION_ERROR, MaterialMasterGeneralDatas_.class, MaterialMasterGeneralDatas_::sizeDimensions);

        /* EAN UPC */
        dataValidator.validateNumber(generalData.getEanUpc(),
                MessageKeys.EAN_UPC_VALIDATION_ERROR, MaterialMasterGeneralDatas_.class, MaterialMasterGeneralDatas_::EAN_UPC);

        /* Product Composition Indicator */
        dataValidator.validateAlphaNumericData(generalData.getProductCompositionIndicator(),
                MessageKeys.PRODUCT_COMPOSITION_INDICATOR_VALIDATION_ERROR, MaterialMasterGeneralDatas_.class, MaterialMasterGeneralDatas_::productCompositionIndicator);

        /* EAN Category */
        dataValidator.validateData(generalData.getEANCategary(),
                MessageKeys.EAN_CATEGORY_VALIDATION_ERROR, MaterialMasterGeneralDatas_.class, MaterialMasterGeneralDatas_::EANCategary);

        /* Packaging Material Group */
        dataValidator.validateData(generalData.getPackagingMaterialGroup(),
                MessageKeys.PACKAGING_MATERIAL_GROUP_VALIDATION_ERROR, MaterialMasterGeneralDatas_.class, MaterialMasterGeneralDatas_::packagingMaterialGroup);

        LoggerHelper.logMethodExit(logger, this.getClass().getSimpleName(), "checkInputsSanitized");
    }
}


