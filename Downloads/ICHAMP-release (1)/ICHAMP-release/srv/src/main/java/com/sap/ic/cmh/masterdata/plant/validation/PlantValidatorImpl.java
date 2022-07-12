package com.sap.ic.cmh.masterdata.plant.validation;

import cds.gen.masterdataservice.Plants;
import cds.gen.masterdataservice.Plants_;
import com.sap.ic.cmh.gen.MessageKeys;
import com.sap.ic.cmh.utils.LoggerHelper;
import com.sap.ic.cmh.utils.datavalidation.DataValidator;
import org.slf4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class PlantValidatorImpl implements PlantValidator {
    public static final Logger logger = LoggerHelper.getLogger(PlantValidatorImpl.class);
    @Autowired
    private DataValidator dataValidator;

    /**
     * Method used to validate and sanitize the plant given details
     * @param plant
     */
    @Override
    public void checkInputsSanitized(Plants plant) {
        LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "checkInputsSanitized");

        /* Plant */
        dataValidator.validateData(plant.getPlant(),
                MessageKeys.PLANT_VALIDATION_ERROR, Plants_.class, Plants_::plant, true);

        /* Plant Name */
        dataValidator.validateData(plant.getPlantName(),
                MessageKeys.PLANT_NAME_VALIDATION_ERROR, Plants_.class, Plants_::plantName);

        /* Plant Name Extension */
        dataValidator.validateData(plant.getPlantNameExtension(),
                MessageKeys.PLANT_NAME_EXTENSION_VALIDATION_ERROR, Plants_.class, Plants_::plantNameExtension);

        /* Company Code */
        dataValidator.validateData(plant.getCompanyCode(),
                MessageKeys.COMPANY_CODE_VALIDATION_ERROR, Plants_.class, Plants_::companyCode);

        /* Customer Number Of Plant */
        dataValidator.validateData(plant.getCustomerNoOfPlant(),
                MessageKeys.CUSTOMER_NO_OF_PLANT_VALIDATION_ERROR, Plants_.class, Plants_::customerNoOfPlant);

        /* Supplier Number Of Plant */
        dataValidator.validateData(plant.getSupplierNoOfPlant(),
                MessageKeys.SUPPLIER_NO_OF_PLANT_VALIDATION_ERROR, Plants_.class, Plants_::supplierNoOfPlant);

        /* Address */
        dataValidator.validateData(plant.getAddress(),
                MessageKeys.ADDRESS_VALIDATION_ERROR, Plants_.class, Plants_::address);

        /* Factory Calendar */
        dataValidator.validateData(plant.getFactoryCalendar(),
                MessageKeys.FACTORY_CALENDAR_VALIDATION_ERROR, Plants_.class, Plants_::factoryCalendar);

        LoggerHelper.logMethodExit(logger, this.getClass().getSimpleName(), "checkInputsSanitized");
    }
}