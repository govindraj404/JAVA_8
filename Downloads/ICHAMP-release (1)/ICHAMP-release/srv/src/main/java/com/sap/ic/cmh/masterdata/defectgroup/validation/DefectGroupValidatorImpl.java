package com.sap.ic.cmh.masterdata.defectgroup.validation;

import cds.gen.masterdataservice.DefectGroups;
import cds.gen.masterdataservice.DefectGroups_;
import com.sap.cds.Result;
import com.sap.cds.services.messages.Messages;
import com.sap.ic.cmh.gen.MessageKeys;
import com.sap.ic.cmh.utils.Constants;
import com.sap.ic.cmh.utils.LoggerHelper;
import com.sap.ic.cmh.utils.datavalidation.DataValidator;
import com.sap.ic.cmh.masterdata.defectcode.repository.DefectCodeRepository;
import org.slf4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class DefectGroupValidatorImpl implements DefectGroupValidator {

    public static final Logger logger = LoggerHelper.getLogger(DefectGroupValidatorImpl.class);

    @Autowired
    private DataValidator dataValidator;

    @Autowired
    DefectCodeRepository defectCodeRepository;

    @Autowired
    private Messages messages;

    @Override
    public void checkInputsSanitized(DefectGroups defectGroups) {
        /* Code */
        dataValidator.validateDataWithSpecialChar(defectGroups.getCode(),
                MessageKeys.DEFECT_GROUP_CODE_VALIDATION_ERROR, DefectGroups_.class, DefectGroups_::code, true,true,Constants.DEFCODERESTRCHAR);

        /* Description */
        dataValidator.validateData(defectGroups.getDescription(),
                MessageKeys.DEFECT_GROUP_DESCRIPTION_VALIDATION_ERROR, DefectGroups_.class, DefectGroups_::description);
    }

    @Override
    public void checkIfDefectCodeExistForDefectGroup(String sDefectGroup) {
        Result defectCode = defectCodeRepository.fetchDefectCodeByDefectGroup(sDefectGroup);
        if (defectCode.first().isPresent()) {
            messages.error(MessageKeys.DEFECT_GROUP_CANNOT_BE_DELETED_AS_ITS_ASSOCIATED_WITH_DEFECT_CODE).target("in",
                    DefectGroups_.class,
                    DefectGroups_::code);

        }
    }

}