package com.sap.ic.cmh.configuration.validations;

import cds.gen.configurationservice.ClaimStatusMappings;
import cds.gen.configurationservice.ClaimStatusMappings_;
import com.sap.cds.services.messages.Messages;
import com.sap.ic.cmh.configuration.persistency.ClaimStatusMappingsDao;
import com.sap.ic.cmh.gen.MessageKeys;
import com.sap.ic.cmh.utils.SecurityValidator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import java.util.Objects;
import java.util.Optional;

@Component
public class ClaimStatusMappingsValidationImpl implements ClaimStatusMappingsValidation {

    @Autowired
    Messages messages;
    @Autowired
    ClaimStatusMappingsDao claimStatusMappingsDao;
    @Autowired
    SecurityValidator securityValidator;

    @Override
    public void validateClaimStatuses(ClaimStatusMappings claimStatusMappings) {
        if(claimStatusMappings.getCode() == null || claimStatusMappings.getCode().isEmpty()){
            messages.error(MessageKeys.CLAIM_STATUS_IS_MANDATORY).target("in", ClaimStatusMappings_.class, ClaimStatusMappings_::code);
        }
        if(claimStatusMappings.getName() == null || claimStatusMappings.getName().isEmpty()){
            messages.error(MessageKeys.CLAIM_STATUS_NAME_IS_MANDATORY).target("in", ClaimStatusMappings_.class, ClaimStatusMappings_::name);
        }
        if(claimStatusMappings.getStatusCode() == null || claimStatusMappings.getStatusCode().isEmpty()){
            messages.error(MessageKeys.CLAIM_STATUS_CODE_IS_MANDATORY).target("in", ClaimStatusMappings_.class, ClaimStatusMappings_::status_code);
        }
        if(claimStatusMappings.getCode() != null && !securityValidator.isValidText(claimStatusMappings.getCode())){
            messages.error(MessageKeys.INVALID_CLAIM_STATUS_CODE).target("in", ClaimStatusMappings_.class,
                    ClaimStatusMappings_::code);
        }
        if(claimStatusMappings.getName() != null && !securityValidator.isValidText(claimStatusMappings.getName())){
            messages.error(MessageKeys.INVALID_CLAIM_STATUS_NAME).target("in", ClaimStatusMappings_.class,
                    ClaimStatusMappings_::name);
        }
        if (claimStatusMappings.getCode() != null && !claimStatusMappings.getCode().isEmpty()) {
            Optional<ClaimStatusMappings> claimStatusMapping = claimStatusMappingsDao.getClaimStatusMappings(claimStatusMappings.getCode());
            if (claimStatusMapping.isPresent() &&
            		!Objects.equals(claimStatusMappings.getId(), claimStatusMapping.get().getId())) {
                    messages.error(MessageKeys.CLAIM_STATUS_MAPPING_ALREADY_EXISTS).target("in", ClaimStatusMappings_.class, ClaimStatusMappings_::code);
            }
        }
    }
}
