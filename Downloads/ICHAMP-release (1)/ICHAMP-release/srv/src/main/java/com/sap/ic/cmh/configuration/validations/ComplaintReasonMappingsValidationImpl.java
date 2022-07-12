package com.sap.ic.cmh.configuration.validations;

import cds.gen.configurationservice.ComplaintReasonMappings_;
import cds.gen.configurationservice.ComplaintReasonMappings;
import cds.gen.masterdataservice.DistributionChannels;
import cds.gen.masterdataservice.Divisions;
import com.sap.ic.cmh.configuration.persistency.ComplaintReasonMappingsDao;
import com.sap.ic.cmh.configuration.persistency.ComplaintReasonsDao;
import com.sap.ic.cmh.configuration.persistency.ItemCategoriesDao;
import com.sap.ic.cmh.configuration.service.ComplaintReasonsService;
import com.sap.ic.cmh.configuration.service.ItemCategoryService;
import com.sap.ic.cmh.gen.MessageKeys;

import com.sap.cds.Result;
import com.sap.cds.services.messages.Messages;
import com.sap.ic.cmh.masterdata.distributionchannel.persistency.DistributionChannelRepository;
import com.sap.ic.cmh.masterdata.division.persistency.DivisionRepository;
import com.sap.ic.cmh.masterdata.salesorganization.persistency.SalesOrganizationRepository;
import com.sap.ic.cmh.utils.LoggerHelper;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;

import org.springframework.stereotype.Component;

@Component
public class ComplaintReasonMappingsValidationImpl implements ComplaintReasonMappingsValidation {

    @Autowired
    ComplaintReasonMappingsDao complaintReasonMappingsDao;

    @Autowired
    DivisionRepository divisionRepository;

    @Autowired
    DistributionChannelRepository distributionChannelRepository;

    @Autowired
    SalesOrganizationRepository salesOrganizationRepository;

    @Autowired
    ItemCategoriesDao itemCategoriesDao;

    @Autowired
    ComplaintReasonsDao complaintReasonsDao;

    @Autowired
    Messages messages;
    @Autowired
    ItemCategoryService itemCategoryService;
    @Autowired
    ComplaintReasonsService complaintReasonsService;

    private static final String COMPLAINT_REASON_MAPPINGS_VALIDATION_IMPL = "ComplaintReasonMappingsValidationImpl";
    private static final Logger logger = LoggerFactory.getLogger(ComplaintReasonMappingsValidationImpl.class);

    /**
     * validate complaint reason map exists
     *
     */
    @Override
    public void validateComplaintReasonMapExist(ComplaintReasonMappings complaintReasonMappings) {
        LoggerHelper.logMethodEntry(logger, COMPLAINT_REASON_MAPPINGS_VALIDATION_IMPL, "validateComplaintReasonMapExist");
        checkItemCategoryExist(complaintReasonMappings.getItemCategoryId());
        checkComplaintReasonExist(complaintReasonMappings.getComplaintReasonId());
        checkSalesOrganizationById(complaintReasonMappings.getSalesOrganizationId());
        checkDistributionChannelExistAndIsMappedWithSalesID(complaintReasonMappings.getDistributionChannelId(),
                complaintReasonMappings.getSalesOrganizationId());
        checkDivisionExistAndIsMappedWithSalesID(complaintReasonMappings.getDivisionId(),
                complaintReasonMappings.getSalesOrganizationId());
        validateComplaintReasonMapExistWithAttributeIds(
                complaintReasonMappings.getSalesOrganizationId(),
                complaintReasonMappings.getDistributionChannelId(),
                complaintReasonMappings.getDivisionId(),
                complaintReasonMappings.getItemCategoryId(),
                complaintReasonMappings.getComplaintReasonId(),
                complaintReasonMappings.getId());
        LoggerHelper.logMethodExit(logger, COMPLAINT_REASON_MAPPINGS_VALIDATION_IMPL, "validateComplaintReasonMapExist");
    }

    /**
     * check if complaint reason map exists with attributes IDs
     *
     */
    public void validateComplaintReasonMapExistWithAttributeIds(String salesOrgID, String distChannelID,
                                                                String divisionID, String complaintID,
                                                                String reasonCodeID, String id) {
        LoggerHelper.logMethodEntry(logger, COMPLAINT_REASON_MAPPINGS_VALIDATION_IMPL, "validateComplaintReasonMapExistWithAttributeIds");
        Result complaintReasonMap = complaintReasonMappingsDao
                .getComplaintReasonMapBasedOnAttributeIDs(salesOrgID, distChannelID,
                        divisionID, complaintID, reasonCodeID);
        if (complaintReasonMap.rowCount() > 0L && !id.equals(complaintReasonMap.first().get().get("ID").toString())) {
            messages.error(MessageKeys.DUPLICATE_COMPLAINT_REASON_MAPPING).target("in", ComplaintReasonMappings_.class,
                    ComplaintReasonMappings_::ID);
        }
        LoggerHelper.logMethodExit(logger, COMPLAINT_REASON_MAPPINGS_VALIDATION_IMPL, "validateComplaintReasonMapExistWithAttributeIds");
    }

    /**
     * check if a valid item category exists
     *
     * @param {@link String} itemCategoryId
     */
    public void checkItemCategoryExist(String itemCategoryId) {
        LoggerHelper.logMethodEntry(logger, COMPLAINT_REASON_MAPPINGS_VALIDATION_IMPL, "checkItemCategoryExist");
        if(itemCategoryId==null || StringUtils.isBlank(itemCategoryId)){
            messages.error(MessageKeys.COMPLAINT_ITEM_CATEGORY_IS_REQUIRED_FIELD)
                    .target("in",ComplaintReasonMappings_.class,
                            ComplaintReasonMappings_::itemCategory_ID);
        } else {
            Result itemCategoryResult = itemCategoriesDao.getComplaintItemCategory(itemCategoryId);
            if(!itemCategoryResult.first().isPresent()) {
                messages.error(MessageKeys.INVALID_COMPLAINT_ITEM_CATEGORY).target("in", ComplaintReasonMappings_.class,
                        ComplaintReasonMappings_::itemCategory_ID);
            }
            else if ( !itemCategoryService.getActive(itemCategoryId)){
                messages.error(MessageKeys.COMPLAINT_REASON_MAP_ITEM_CATEGORY_INACTIVE_ERROR).target("in", ComplaintReasonMappings_.class,ComplaintReasonMappings_::itemCategory_ID);
            }
        }
        LoggerHelper.logMethodExit(logger, COMPLAINT_REASON_MAPPINGS_VALIDATION_IMPL, "checkItemCategoryExist");
    }

    /**
     * check if a valid complaint reason exists
     *
     * @param {@link String} complaintReasonID
     */
    public void checkComplaintReasonExist(String complaintReasonID) {
        LoggerHelper.logMethodEntry(logger, COMPLAINT_REASON_MAPPINGS_VALIDATION_IMPL, "checkComplaintReasonExist");
        if(complaintReasonID==null || StringUtils.isBlank(complaintReasonID)){
            messages.error(MessageKeys.COMPLAINT_REASON_IS_REQUIRED_FIELD)
                    .target("in",ComplaintReasonMappings_.class,
                            ComplaintReasonMappings_::complaintReason_ID);
        } else {
            Result reasonResult = complaintReasonsDao
                    .getComplaintReasonBasedOnID(complaintReasonID);
            if(!reasonResult.first().isPresent()) {
                messages.error(MessageKeys.INVALID_COMPLAINT_REASON).target("in", ComplaintReasonMappings_.class,
                        ComplaintReasonMappings_::complaintReason_ID);
            }
           else if ( !complaintReasonsService.getActive(complaintReasonID)){
                messages.error(MessageKeys.COMPLAINT_REASON_MAP_COMPLAINT_REASON_INACTIVE_ERROR).target("in", ComplaintReasonMappings_.class,ComplaintReasonMappings_::itemCategory_ID);
            }

        }
        LoggerHelper.logMethodExit(logger, COMPLAINT_REASON_MAPPINGS_VALIDATION_IMPL, "checkComplaintReasonExist");
    }

    /**
     * check if division exists and is mapped to the sales organisation ID
     *
     */
    public void checkDivisionExistAndIsMappedWithSalesID(String divisionId, String salesId) {
        LoggerHelper.logMethodEntry(logger, COMPLAINT_REASON_MAPPINGS_VALIDATION_IMPL, "checkDivisionExistAndIsMappedWithSalesID");
        if (null == divisionId || StringUtils.isBlank(divisionId)) {
            messages.error(MessageKeys.DIVISION_IS_REQUIRED_FIELD)
                    .target("in", ComplaintReasonMappings_.class,
                            ComplaintReasonMappings_::division_ID);
        } else {
            Result divisionResult = divisionRepository.getDivisionIDandSalesIDById(divisionId);
            if (divisionResult.first().isPresent() && !divisionResult.listOf(Divisions.class).get(0)
                    .get(Divisions.SALES_ORGANIZATION_ID_ID).equals(salesId)) {
                messages.error(MessageKeys.DIVISION_SPECIFIED_IS_NOT_RELATED_TO_SALES_ID)
                        .target("in", ComplaintReasonMappings_.class,
                                ComplaintReasonMappings_::division_ID);
            } else if (!divisionResult.first().isPresent()) {
                messages.error(MessageKeys.DIVISION_VALIDATION_ERROR)
                        .target("in",ComplaintReasonMappings_.class,
                                ComplaintReasonMappings_::division_ID);
            }
        }
        LoggerHelper.logMethodExit(logger, COMPLAINT_REASON_MAPPINGS_VALIDATION_IMPL, "checkDivisionExistAndIsMappedWithSalesID");
    }

    /**
     * check if sales distribution channel exists and is mapped to the sales organisation ID
     *
     */
    public void checkDistributionChannelExistAndIsMappedWithSalesID(String distributionChannelId, String salesId) {
        LoggerHelper.logMethodEntry(logger, COMPLAINT_REASON_MAPPINGS_VALIDATION_IMPL, "checkDistributionChannelExistAndIsMappedWithSalesID");
        if(null == distributionChannelId || StringUtils.isBlank(distributionChannelId)) {
            messages.error(MessageKeys.DISTRIBUTION_CHANNEL_IS_REQUIRED_FIELD)
                    .target("in",ComplaintReasonMappings_.class,
                            ComplaintReasonMappings_::distributionChannel_ID);
        } else {
            Result distributionChannelResult = distributionChannelRepository.getDistributionChannelById(distributionChannelId);
            if (distributionChannelResult.first().isPresent() && !distributionChannelResult.listOf(DistributionChannels.class).get(0)
                    .get(DistributionChannels.SALES_ORGANIZATION_ID_ID).equals(salesId)) {
                messages.error(MessageKeys.DISTRIBUTION_CHANNEL_SPECIFIED_IS_NOT_RELATED_TO_SALES_ID)
                        .target("in", ComplaintReasonMappings_.class,
                                ComplaintReasonMappings_::distributionChannel_ID);
            } else if (!distributionChannelResult.first().isPresent()) {
                messages.error(MessageKeys.DISTRIBUTION_CHANNEL_DOES_NOT_EXIST)
                        .target("in",ComplaintReasonMappings_.class,
                                ComplaintReasonMappings_::distributionChannel_ID);
            }
        }
        LoggerHelper.logMethodExit(logger, COMPLAINT_REASON_MAPPINGS_VALIDATION_IMPL, "checkDistributionChannelExistAndIsMappedWithSalesID");
    }

    /**
     * check if sales organisation exists
     *
     */
    public void checkSalesOrganizationById(String salesOrganizationId) {
        LoggerHelper.logMethodEntry(logger, COMPLAINT_REASON_MAPPINGS_VALIDATION_IMPL, "checkSalesOrganizationById");

        if(null == salesOrganizationId || StringUtils.isBlank(salesOrganizationId)){
            messages.error(MessageKeys.SALES_ORGANIZATION_IS_REQUIRED_FIELD).target("in",ComplaintReasonMappings_.class,
                    ComplaintReasonMappings_::salesOrganization_ID);

        } else {
            Result salesOrganizationByIdResult = salesOrganizationRepository
                    .getSalesOrganizationById(salesOrganizationId);
            if(!salesOrganizationByIdResult.first().isPresent()) {
                messages.error(MessageKeys.SALES_ORGANIZATION_DOES_NOT_EXIST).target("in", ComplaintReasonMappings_.class,
                        ComplaintReasonMappings_::salesOrganization_ID);
            }
        }
        LoggerHelper.logMethodExit(logger, COMPLAINT_REASON_MAPPINGS_VALIDATION_IMPL, "checkSalesOrganizationById");
    }
}

