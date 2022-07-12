package com.sap.ic.cmh.configuration.validations.sourcereferencetypemappings;

import java.util.List;
import java.util.Optional;
import com.sap.cds.Result;
import com.sap.cds.Row;
import com.sap.cds.services.messages.Messages;
import com.sap.cds.services.messages.Message.Severity;
import com.sap.ic.cmh.configuration.service.sourcereferencetypemappings.SourceReferenceTypeMappingService;
import com.sap.ic.cmh.configuration.validations.ConfigurationFieldsValidation;
import com.sap.ic.cmh.configuration.validations.MasterDataValidation;
import com.sap.ic.cmh.gen.MessageKeys;
import com.sap.ic.cmh.utils.LoggerHelper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import cds.gen.configurationservice.SourceReferenceTypeMappings;
import cds.gen.configurationservice.SourceReferenceTypeMappings_;
import cds.gen.configurationservice.SourceReferenceTypes;
import io.micrometer.core.instrument.util.StringUtils;

@Component
public class SourceReferenceTypeMappingValidationImpl implements SourceReferenceTypeMappingValidation {
    
    @Autowired
    Messages messages;

    @Autowired
    SourceReferenceTypeMappingService sourceReferenceTypeMappingService;

    @Autowired
    MasterDataValidation masterDataValidation;

    @Autowired
    ConfigurationFieldsValidation configurationFieldsValidation;

    private static final Logger logger = LoggerFactory.getLogger(SourceReferenceTypeMappingValidationImpl.class);
    private static final String SOURCE_REFERENCE_TYPE_VALIDATION_IMPL = "SourceReferenceTypeMappingValidationImpl";

    /**
     * Validate Source Reference Type Mapping and its individual attributes
     *
     * @param {@link SourceReferenceTypeMappings} sourceReferenceTypeMapping
     *
     * @public
     */
    @Override
    public void validateSourceReferenceTypeMapping(SourceReferenceTypeMappings sourceReferenceTypeMapping){
        LoggerHelper.logMethodEntry(logger, SOURCE_REFERENCE_TYPE_VALIDATION_IMPL, "validateSourceReferenceTypeMapping");

        masterDataValidation.validateSalesOrganization(sourceReferenceTypeMapping.getSalesOrganizationId(),SourceReferenceTypeMappings_.class, SourceReferenceTypeMappings_::salesOrganization_ID);
		masterDataValidation.validateDistributeChannel(sourceReferenceTypeMapping.getDistributionChannelId(),sourceReferenceTypeMapping.getSalesOrganizationId(),SourceReferenceTypeMappings_.class, SourceReferenceTypeMappings_::distributionChannel_ID);
		masterDataValidation.validateDivision(sourceReferenceTypeMapping.getDivisionId(),sourceReferenceTypeMapping.getSalesOrganizationId(),SourceReferenceTypeMappings_.class, SourceReferenceTypeMappings_::division_ID);
        configurationFieldsValidation.validateItemCategory(sourceReferenceTypeMapping.getItemCategoryId(),SourceReferenceTypeMappings_.class, SourceReferenceTypeMappings_::itemCategory_ID);
        validateComplaintType(sourceReferenceTypeMapping.getComplaintTypeId());
        validateSourceSystem(sourceReferenceTypeMapping.getSourceSystem());

        if (messages.stream().noneMatch(message -> message.getSeverity() == Severity.ERROR)) {
            Result sourceReferenceTypeMappingsResult = sourceReferenceTypeMappingService.getSourceReferenceTypeMappingBasedOnValues(sourceReferenceTypeMapping);
            Optional<Row> sourceReferenceTypeMappingFirst = sourceReferenceTypeMappingsResult.first();
            if(sourceReferenceTypeMappingFirst.isPresent() && !sourceReferenceTypeMapping.getId().equals(sourceReferenceTypeMappingFirst.get().get("ID").toString())) {
                messages.error(MessageKeys.ONLY_ONE_SOURCE_REFERENCE_TYPE_MAPPING_PER_ATTRIBUTES).target("in", SourceReferenceTypeMappings_.class,
                SourceReferenceTypeMappings_::sourceSystem);
            }
        }
        LoggerHelper.logMethodExit(logger, SOURCE_REFERENCE_TYPE_VALIDATION_IMPL, "validateSourceReferenceTypeMapping");
    }

    	/**
     * Validate Complaint Type
     * @param complaintTypeId
     */
	public void validateComplaintType(String complaintTypeId) {
        if(StringUtils.isBlank(complaintTypeId)){
            messages.error(MessageKeys.COMPLAINT_TYPE_IS_MANDATORY).target("in", SourceReferenceTypeMappings_.class,
            SourceReferenceTypeMappings_::complaintType_ID);
        }
    }

     	/**
     * Validate Reference Type
     * @param referenceTypeList
     */
	public void validateReferenceType(List<SourceReferenceTypes> list) {
        if(list.isEmpty()){
            messages.error(MessageKeys.REFERENCE_TYPES_IS_MANDATORY).target("in", SourceReferenceTypeMappings_.class,
            SourceReferenceTypeMappings_::referenceTypes);
        }
    }

      	/**
     * Validate Source System
     * @param sourceSystem
     */
	public void validateSourceSystem(String sourceSytem) {
        if(StringUtils.isBlank(sourceSytem)){
            messages.error(MessageKeys.SOURCE_SYSTEM_IS_MANDATORY).target("in", SourceReferenceTypeMappings_.class,
            SourceReferenceTypeMappings_::sourceSystem);
        }
    }
}
