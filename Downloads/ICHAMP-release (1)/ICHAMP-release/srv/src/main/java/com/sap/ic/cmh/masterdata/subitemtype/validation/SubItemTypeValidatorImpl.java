package com.sap.ic.cmh.masterdata.subitemtype.validation;

import cds.gen.masterdataservice.ItemTypes;
import cds.gen.masterdataservice.SubItemTypes;
import cds.gen.masterdataservice.SubItemTypes_;
import com.sap.cds.Result;
import com.sap.cds.services.messages.Messages;
import com.sap.ic.cmh.gen.MessageKeys;
import com.sap.ic.cmh.masterdata.common.persistency.MasterDataDao;
import com.sap.ic.cmh.utils.LoggerHelper;
import com.sap.ic.cmh.utils.datavalidation.DataValidator;
import org.slf4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class SubItemTypeValidatorImpl implements SubItemTypeValidator {

    public static final Logger logger = LoggerHelper.getLogger(SubItemTypeValidatorImpl.class);
    @Autowired
    private DataValidator dataValidator;

    @Autowired
    private Messages messages;

    /* MasterDataDao instance */
    @Autowired
    private MasterDataDao masterDataDao;

    /**
     * Method used to validate the SubItemType Fields
     * @param subItemType
     */
    
    public void validateSubItemTypeFields(SubItemTypes subItemType) {
        checkInputsSanitized(subItemType);
        validateItemTypeCode(subItemType.getItemTypeCode());
    }

    /**
     * Method used to validate and sanitize the SubItemType given details
     * @param subItemType
     */
    
    public void checkInputsSanitized(SubItemTypes subItemType) {
        LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "checkInputsSanitized");

        /* Code */
        dataValidator.validateData(subItemType.getCode(),
                MessageKeys.SUB_ITEM_TYPE_CODE_VALIDATION_ERROR, SubItemTypes_.class, SubItemTypes_::code, true);

        /* Description */
        dataValidator.validateData(subItemType.getDescription(),
                MessageKeys.SUB_ITEM_TYPE_DESCRIPTION_VALIDATION_ERROR, SubItemTypes_.class, SubItemTypes_::description);
      
        LoggerHelper.logMethodExit(logger, this.getClass().getSimpleName(), "checkInputsSanitized");
    }

    /**
     * Method used to validate the Sub Item Type code
     * @param itemTypeCode
     */
    public void validateItemTypeCode(String itemTypeCode) {
        Result aItemTypes = masterDataDao.getItemTypeCode(itemTypeCode);        
        if (aItemTypes == null || !aItemTypes.first(ItemTypes.class).isPresent()) {
            messages.error(MessageKeys.ITEM_TYPE_CODE_VALIDATION_ERROR).target("in", SubItemTypes_.class, SubItemTypes_::itemType_code);
        }
    }
}