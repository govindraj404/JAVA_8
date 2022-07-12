package com.sap.ic.cmh.configuration.validations.targetreferencetypemappings;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import com.sap.cds.Result;
import com.sap.cds.Row;
import com.sap.cds.ql.Select;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.messages.Messages;
import com.sap.cds.services.messages.Message.Severity;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.ic.cmh.configuration.service.ComplaintTypeConfigurationService;
import com.sap.ic.cmh.configuration.service.ItemCategoryService;
import com.sap.ic.cmh.configuration.service.targetreferencetypemappings.TargetReferenceTypeMappingService;
import com.sap.ic.cmh.configuration.validations.ConfigurationFieldsValidation;
import com.sap.ic.cmh.configuration.validations.MasterDataValidation;
import com.sap.ic.cmh.gen.MessageKeys;
import com.sap.ic.cmh.utils.LoggerHelper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import cds.gen.configurationservice.TargetReferenceTypeMappings;
import cds.gen.configurationservice.TargetReferenceTypeMappings_;
import cds.gen.configurationservice.TargetReferenceTypes;
import cds.gen.configurationservice.TargetTypes;
import cds.gen.configurationservice.TargetTypes_;
import io.micrometer.core.instrument.util.StringUtils;

@Component
public class TargetReferenceTypeMappingValidationImpl implements TargetReferenceTypeMappingValidation {

    @Autowired
    Messages messages;

    @Autowired
    MasterDataValidation masterDataValidation;

    @Autowired
    ConfigurationFieldsValidation configurationFieldsValidation;

    @Autowired
    TargetReferenceTypeMappingService targetReferenceTypeMappingService;

    @Autowired
    PersistenceService db;

    @Autowired
    ComplaintTypeConfigurationService service;

    @Autowired
    ItemCategoryService iService;

    private static final Logger logger = LoggerFactory.getLogger(TargetReferenceTypeMappingValidationImpl.class);
    private static final String TARGET_REFERENCE_TYPE_VALIDATION_IMPL = "TargetReferenceTypeMappingValidationImpl";

    /**
     * Validate Target Reference Type Mapping and its individual attributes
     *
     * @param {@link TargetReferenceTypeMapping} targetReferenceTypeMapping
     *
     * @public
     */

    @Override
    public void validateTargetReferenceTypeMapping(TargetReferenceTypeMappings targetReferenceTypeMapping) {
        LoggerHelper.logMethodEntry(logger, TARGET_REFERENCE_TYPE_VALIDATION_IMPL,
                "validateTargetReferenceTypeMapping");

        masterDataValidation.validateSalesOrganization(targetReferenceTypeMapping.getSalesOrganizationId(),
                TargetReferenceTypeMappings_.class, TargetReferenceTypeMappings_::salesOrganization_ID);
        masterDataValidation.validateDistributeChannel(targetReferenceTypeMapping.getDistributionChannelId(),
                targetReferenceTypeMapping.getSalesOrganizationId(), TargetReferenceTypeMappings_.class,
                TargetReferenceTypeMappings_::distributionChannel_ID);
        masterDataValidation.validateDivision(targetReferenceTypeMapping.getDivisionId(),
                targetReferenceTypeMapping.getSalesOrganizationId(), TargetReferenceTypeMappings_.class,
                TargetReferenceTypeMappings_::division_ID);
        configurationFieldsValidation.validateItemCategory(targetReferenceTypeMapping.getItemCategoryId(),
                TargetReferenceTypeMappings_.class, TargetReferenceTypeMappings_::itemCategory_ID);
        validateItemCategoryId(targetReferenceTypeMapping.getItemCategoryId());
        validateComplaintType(targetReferenceTypeMapping.getComplaintTypeId());
        validateTargetType(targetReferenceTypeMapping.getTargetTypes());

        if (messages.stream().noneMatch(message -> message.getSeverity() == Severity.ERROR)) {
            Result targetReferenceTypeMappingsResult = targetReferenceTypeMappingService
                    .getTargetReferenceTypeMappingBasedOnValues(targetReferenceTypeMapping);
            Optional<Row> targetReferenceTypeMappingFirst = targetReferenceTypeMappingsResult.first();
            if (targetReferenceTypeMappingFirst.isPresent() && !targetReferenceTypeMapping.getId()
                    .equals(targetReferenceTypeMappingFirst.get().get("ID").toString())) {
                messages.error(MessageKeys.ONLY_ONE_TARGET_REFERENCE_TYPE_MAPPING_PER_ATTRIBUTES).target("in",
                        TargetReferenceTypeMappings_.class, TargetReferenceTypeMappings_::distributionChannel_ID);
            }
        }
        LoggerHelper.logMethodExit(logger, TARGET_REFERENCE_TYPE_VALIDATION_IMPL, "validateTargetReferenceTypeMapping");
    }

    private void validateTargetType(List<TargetReferenceTypes> targetTypes) {
        Map<String, String> targetTypeMap = new HashMap<>();
        if (targetTypes.isEmpty()) {
            messages.error(MessageKeys.TARGET_TYPE_IS_MANDATORY).target("in", TargetReferenceTypeMappings_.class,
                    TargetReferenceTypeMappings_::targetTypes);
        } else {
            for (TargetReferenceTypes targetType : targetTypes) {

                String targetTypeID = targetType.getTargetTypeId();
                CqnSelect select = Select.from(TargetTypes_.class)
                        .columns(a -> a.code(), a -> a.targetDocumentCategory().expand())
                        .where(b -> b.ID().eq(targetTypeID));
                Result result = db.run(select);
                TargetTypes targetTypesResult = result.first().isPresent() ? result.listOf(TargetTypes.class).get(0)
                        : null;
                if (targetTypesResult != null) {
                    if (targetTypeMap.isEmpty()) {
                        targetTypeMap.put(targetTypesResult.getTargetDocumentCategory().getCode(),
                                targetTypesResult.getTargetDocumentCategory().getDescription());
                    } else if (targetTypeMap.containsKey(targetTypesResult.getTargetDocumentCategory().getCode())) {
                        messages.error(MessageKeys.DUPLICATE_TARGET_DOCUMENT_CATEGORY).target("in",
                                TargetReferenceTypeMappings_.class, TargetReferenceTypeMappings_::targetTypes);

                    }

                }
            }

        }
    }

    /**
     * Validate Complaint Type
     * 
     * @param complaintTypeId
     */
    private void validateComplaintType(String complaintTypeId) {
        LoggerHelper.logMethodEntry(logger, TARGET_REFERENCE_TYPE_VALIDATION_IMPL, "validateComplaintType");
        if (StringUtils.isBlank(complaintTypeId)) {
            messages.error(MessageKeys.COMPLAINT_TYPE_IS_MANDATORY).target("in", TargetReferenceTypeMappings_.class,
                    TargetReferenceTypeMappings_::complaintType_ID);
        }
        else if
            ( !service.getActive(complaintTypeId))
            {
                messages.error(MessageKeys.COMPLAINT_TYPE_INACTIVE_ERROR).target("in", TargetReferenceTypeMappings_.class, TargetReferenceTypeMappings_::complaintType_ID);
            }
        LoggerHelper.logMethodExit(logger, TARGET_REFERENCE_TYPE_VALIDATION_IMPL, "validateComplaintType");
    }
    /**
     * validate  item category
     *
     * @param {@link String} itemCategoryId
     *
     * @public
     */
    private void validateItemCategoryId(String itemCategoryId) {
        LoggerHelper.logMethodEntry(logger, TARGET_REFERENCE_TYPE_VALIDATION_IMPL, "validateItemCategoryId");
         if ( !StringUtils.isBlank(itemCategoryId) && !iService.getActive(itemCategoryId))
        {
            messages.error(MessageKeys.ITEM_CATEGORY_INACTIVE_ERROR).target("in", TargetReferenceTypeMappings_.class, TargetReferenceTypeMappings_::itemCategory_ID);
        }
        LoggerHelper.logMethodExit(logger, TARGET_REFERENCE_TYPE_VALIDATION_IMPL, "validateItemCategoryId");

    }
}
