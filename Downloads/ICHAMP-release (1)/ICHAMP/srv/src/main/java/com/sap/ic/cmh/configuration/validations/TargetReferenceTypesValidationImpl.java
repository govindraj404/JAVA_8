package com.sap.ic.cmh.configuration.validations;

import java.util.HashSet;
import java.util.List;
import com.sap.cds.services.messages.Messages;
import com.sap.ic.cmh.configuration.persistency.TargetReferenceTypeDao;
import com.sap.ic.cmh.configuration.service.TargetTypeConfigurationService;
import com.sap.ic.cmh.gen.MessageKeys;
import org.springframework.stereotype.Component;
import org.springframework.beans.factory.annotation.Autowired;
import cds.gen.configurationservice.TargetReferenceTypeMappings;
import cds.gen.configurationservice.TargetReferenceTypes;
import cds.gen.configurationservice.TargetReferenceTypes_;
import io.micrometer.core.instrument.util.StringUtils;
import com.sap.ic.cmh.utils.SecurityValidator;

@Component
public class TargetReferenceTypesValidationImpl implements TargetReferenceTypesValidation {

    @Autowired
    Messages messages;

    @Autowired
    TargetReferenceTypeDao targetReferenceTypeDao;

    @Autowired
    SecurityValidator securityValidator;

    @Autowired
    TargetTypeConfigurationService targetTypeConfigurationService;

    /**
     * Validate Target Reference Types and its individual attributes
     *
     * @param {@link TargetReferenceTypes} targetReferenceTypes
     *
     * @public
     */
    @Override
    public void validateTargetReferenceTypes(TargetReferenceTypeMappings targetReferenceTypeMappings) {
        List<TargetReferenceTypes> targetReferenceTypes = targetReferenceTypeMappings.getTargetTypes();
        HashSet<String> set = new HashSet<>();
        for(TargetReferenceTypes targetRefType : targetReferenceTypes){
            validateTargetReferenceTypesNotNull(targetRefType);
            if (targetRefType.getTargetTypeId()!=null && !set.add(targetRefType.getTargetTypeId())) {
                messages.error(MessageKeys.TARGET_TYPE_EXIST).target("in", TargetReferenceTypes_.class,
                        TargetReferenceTypes_::targetType_ID);
            }
            if (!targetTypeConfigurationService.getActive(targetRefType.getTargetTypeId())){
                messages.error(MessageKeys.TARGET_REFERENCE_TYPE_TARGET_TYPE_INACTIVE_ERROR).target("in", TargetReferenceTypes_.class,TargetReferenceTypes_::targetType_ID);
            }
        }
    }

    public void validateTargetReferenceTypesNotNull(TargetReferenceTypes targetReferenceType){

        if(StringUtils.isBlank(targetReferenceType.getTargetTypeId())){
            messages.error(MessageKeys.TARGET_TYPE_IS_MANDATORY).target("in", TargetReferenceTypes_.class,
            TargetReferenceTypes_::targetType_ID);
        }
        if(StringUtils.isBlank(targetReferenceType.getDestinationSystem())){
            messages.error(MessageKeys.DESTINATION_SYSTEM_IS_MANDATORY).target("in", TargetReferenceTypes_.class,
            TargetReferenceTypes_::destinationSystem);
        }

    }

}
