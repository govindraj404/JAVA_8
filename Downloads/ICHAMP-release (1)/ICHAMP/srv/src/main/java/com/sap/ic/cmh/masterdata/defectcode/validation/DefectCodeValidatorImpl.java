package com.sap.ic.cmh.masterdata.defectcode.validation;

import cds.gen.masterdataservice.DefectCodes;
import cds.gen.masterdataservice.DefectCodes_;
import cds.gen.masterdataservice.DefectGroups;
import com.sap.cds.Result;
import com.sap.cds.services.messages.Messages;
import com.sap.ic.cmh.gen.MessageKeys;
import com.sap.ic.cmh.masterdata.common.persistency.MasterDataDao;
import com.sap.ic.cmh.utils.Constants;
import com.sap.ic.cmh.utils.LoggerHelper;
import com.sap.ic.cmh.utils.datavalidation.DataValidator;
import org.slf4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class DefectCodeValidatorImpl implements DefectCodeValidator {

    public static final Logger logger = LoggerHelper.getLogger(DefectCodeValidatorImpl.class);

    @Autowired
    private DataValidator dataValidator;

    @Autowired
    private Messages messages;

    @Autowired
    private MasterDataDao masterDataDao;

    @Override
    public void checkInputsSanitized(DefectCodes defectCodes) {
        /* Code */
        dataValidator.validateDataWithSpecialChar(defectCodes.getCode(),
                MessageKeys.DEFECT_CODE_VALIDATION_ERROR, DefectCodes_.class, DefectCodes_::code, true,true,Constants.DEFCODERESTRCHAR);

        /* Description */
        dataValidator.validateData(defectCodes.getDescription(),
                MessageKeys.DEFECT_CODE_DESCRIPTION_VALIDATION_ERROR, DefectCodes_.class, DefectCodes_::description);

        validateDefectGroup(defectCodes.getDefectGroupCode());
    }

    /**
     * Method used to validate the Defects Group
     *
     * @param defectGroup
     */
    private void validateDefectGroup(String defectGroup) {
        Result aItemTypes = masterDataDao.getDefectGroup(defectGroup);
        if (aItemTypes == null || !aItemTypes.first(DefectGroups.class).isPresent()) {
            messages.error(MessageKeys.DEFECT_GROUP_NOT_EXIST).target("in", DefectCodes_.class,
                    DefectCodes_::defectGroup_code);
        }
    }
}