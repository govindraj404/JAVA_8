package com.sap.ic.cmh.configuration.handler;

import cds.gen.configurationservice.ItemCategories;
import cds.gen.configurationservice.ItemCategories_;
import com.sap.cds.Row;
import com.sap.cds.services.auditlog.Action;
import com.sap.cds.services.cds.CdsService;
import com.sap.cds.services.draft.DraftNewEventContext;
import com.sap.cds.services.draft.DraftService;
import com.sap.cds.services.handler.EventHandler;
import com.sap.cds.services.handler.annotations.After;
import com.sap.cds.services.handler.annotations.Before;
import com.sap.cds.services.handler.annotations.On;
import com.sap.cds.services.handler.annotations.ServiceName;
import com.sap.cds.services.messages.Messages;
import com.sap.ic.cmh.auditlog.AuditLogHelper;
import com.sap.ic.cmh.configuration.service.ItemCategoryService;
import com.sap.ic.cmh.configuration.validations.ItemCategoryValidation;
import com.sap.ic.cmh.utils.Constants;
import com.sap.ic.cmh.utils.LoggerHelper;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.Optional;
import java.util.stream.Stream;

@Component
@ServiceName("ConfigurationService")
public class ItemCategoryHandler implements EventHandler {


    @Autowired
    Messages messages;

    @Autowired
    ItemCategoryValidation validation;

    @Autowired
    ItemCategoryService service;

    @Autowired
    private AuditLogHelper<ItemCategories> auditLogHelper;

    private static final Logger logger = LoggerFactory.getLogger(ItemCategoryHandler.class);
    private static final String ITEM_CATEGORY_HANDLER = "ItemCategoryHandler";

    /**
     * Before draft new event is setting boolean values in the mandatory fields
     *
     * @param {@link DraftNewEventContext,ItemCategories}
     *               context,itemCategory
     * @public
     */
    @Before(event = DraftService.EVENT_DRAFT_NEW, entity = ItemCategories_.CDS_NAME)
    public void beforeCreateItemCategoryDraft(DraftNewEventContext context, ItemCategories itemCategory) {
        LoggerHelper.logMethodEntry(logger, ITEM_CATEGORY_HANDLER, "beforeCreateItemCategoryDraft");
        if (itemCategory.getMaterialEnteredManually() == null) {
            itemCategory.setMaterialEnteredManually(false);
        }
        if (itemCategory.getReceivedQuantityEditable() == null) {
            itemCategory.setReceivedQuantityEditable(false);
        }
        if (itemCategory.getReturnQuantityEditable() == null) {
            itemCategory.setReturnQuantityEditable(false);
        }
        if (itemCategory.getIndividualComplaint() == null) {
            itemCategory.setIndividualComplaint(false);
        }
        if (itemCategory.getExternalReferenceMandatory() == null) {
            itemCategory.setExternalReferenceMandatory(false);
        }
        if (itemCategory.getExternalReferenceCheckedForDuplication() == null) {
            itemCategory.setExternalReferenceCheckedForDuplication(false);
        }
        if (itemCategory.getCreditDebitAmountEnteredManually() == null) {
            itemCategory.setCreditDebitAmountEnteredManually(false);
            itemCategory.setIsFieldControlEnableConditionType(Constants.FIELD_CONTROL_READ_ONLY);
        }
        LoggerHelper.logMethodExit(logger, ITEM_CATEGORY_HANDLER, "beforeCreateItemCategoryDraft");
    }

    /**
     * Before create event is performing input validation
     *
     * @param {@link ItemCategories} itemCategory
     * @public
     */
    @Before(event = {CdsService.EVENT_CREATE}, entity = ItemCategories_.CDS_NAME)
    public void beforeCreateItemCategory(ItemCategories itemCategory) {
        LoggerHelper.logMethodEntry(logger, ITEM_CATEGORY_HANDLER, "beforeCreateItemCategory");
        validation.validateItemCategories(itemCategory);
        messages.throwIfError();
        LoggerHelper.logMethodExit(logger, ITEM_CATEGORY_HANDLER, "beforeCreateItemCategory");
    }

    /**
     * On create event is generation sequence number
     *
     * @param {@link ItemCategories} itemCategory
     * @public
     */
    @On(event = {CdsService.EVENT_CREATE}, entity = ItemCategories_.CDS_NAME)
    public void onCreateItemCategory(ItemCategories itemCategory) {
        LoggerHelper.logMethodEntry(logger, ITEM_CATEGORY_HANDLER, "onCreateItemCategory");
        Optional<Row> itemCategoryFirst = service.getItemCategories().first();
        Integer sequenceNumber = (itemCategoryFirst.isPresent() && null != itemCategoryFirst.get().get("identifier")) ? Integer.parseInt(itemCategoryFirst.get().get("identifier").toString()) + 1 : 1;
        itemCategory.setIdentifier(sequenceNumber);
        LoggerHelper.logMethodExit(logger, ITEM_CATEGORY_HANDLER, "onCreateItemCategory");
    }

    /**
     * Update create event is performing input validation on update
     *
     * @param {@link ItemCategories} itemCategory
     * @public
     */
    @Before(event = {CdsService.EVENT_UPDATE}, entity = ItemCategories_.CDS_NAME)
    public void beforeUpdateItemCategory(ItemCategories itemCategory) {
        LoggerHelper.logMethodEntry(logger, ITEM_CATEGORY_HANDLER, "beforeUpdateItemCategory");
        validation.validateItemCategories(itemCategory);
        messages.throwIfError();
        LoggerHelper.logMethodExit(logger, ITEM_CATEGORY_HANDLER, "beforeUpdateItemCategory");
    }


    /**
     * After read event is enabling/disabling fields based on conditions
     *
     * @param {@link Stream<ItemCategories>} itemCategories
     * @public
     */
    @After(event = CdsService.EVENT_READ, entity = ItemCategories_.CDS_NAME)
    public void afterReadItemCategory(Stream<ItemCategories> itemCategories) {
        LoggerHelper.logMethodEntry(logger, ITEM_CATEGORY_HANDLER, "afterReadItemCategory");
        itemCategories.forEach(b -> {
            b.setIsFieldControlEnableMaterial(Constants.FIELD_CONTROL_READ_ONLY);
            if (b.getMaterialEnteredManually() != null && b.getMaterialEnteredManually().equals(Boolean.TRUE)
                    || StringUtils.isBlank(b.getReferenceDocumentCategoryCode())) {
                b.setIsFieldControlEnableMaterial(Constants.FIELD_CONTROL_OPTIONAL);
            }
            b.setIsFieldControlEnableConditionType(Constants.FIELD_CONTROL_READ_ONLY);
            if (b.getCreditDebitAmountEnteredManually() != null
                    && b.getCreditDebitAmountEnteredManually().equals(Boolean.TRUE)) {
                b.setIsFieldControlEnableConditionType(Constants.FIELD_CONTROL_OPTIONAL);
            }
            b.setIsInActive(true);
            if (b.getIsActiveEntity() != null && b.getHasDraftEntity() != null && b.getIsActive() != null &&
                    Boolean.TRUE.equals(b.getIsActiveEntity()) && Boolean.FALSE.equals(b.getHasDraftEntity())) {
                b.setIsInActive(!b.getIsActive());
            }
        });
        LoggerHelper.logMethodExit(logger, ITEM_CATEGORY_HANDLER, "afterReadItemCategory");
    }

    /**
     * Update patch event is setting values on change
     *
     * @param {@link ItemCategories} itemCategory
     * @public
     */
    @Before(event = DraftService.EVENT_DRAFT_PATCH, entity = ItemCategories_.CDS_NAME)
    public void beforePatchItemCategory(ItemCategories itemCategory) {
        LoggerHelper.logMethodEntry(logger, ITEM_CATEGORY_HANDLER, "beforePatchItemCategory");
        if (itemCategory.getMaterialEnteredManually() != null && itemCategory.getMaterialEnteredManually().equals(Boolean.FALSE)
                || !StringUtils.isBlank(itemCategory.getReferenceDocumentCategoryCode())) {
            itemCategory.setMaterialId(null);
        }
        if (itemCategory.getCreditDebitAmountEnteredManually() != null
                && itemCategory.getCreditDebitAmountEnteredManually().equals(Boolean.FALSE)) {
            itemCategory.setConditionType(null);
        }
        LoggerHelper.logMethodExit(logger, ITEM_CATEGORY_HANDLER, "beforePatchItemCategory");
    }

    /**
     * After Update event of ItemCategories Object perform validations
     *
     * @public
     */
    @After(event = {CdsService.EVENT_UPDATE}, entity = ItemCategories_.CDS_NAME)
    public void afterUpdateItemCategory(ItemCategories itemCategory) {
        LoggerHelper.logMethodEntry(logger, ITEM_CATEGORY_HANDLER, "afterUpdateItemCategory");
        logUpsert(Action.UPDATE, itemCategory);
        LoggerHelper.logMethodExit(logger, ITEM_CATEGORY_HANDLER, "afterUpdateItemCategory");
    }

    /**
     * After Create event of ItemCategories Object perform validations
     *
     * @public
     */
    @After(event = {CdsService.EVENT_CREATE}, entity = ItemCategories_.CDS_NAME)
    public void afterCreateItemCategory(ItemCategories itemCategory) {
        LoggerHelper.logMethodEntry(logger, ITEM_CATEGORY_HANDLER, "afterCreateItemCategory");
        logUpsert(Action.CREATE, itemCategory);
        LoggerHelper.logMethodExit(logger, ITEM_CATEGORY_HANDLER, "afterCreateItemCategory");
    }

    /**
     * To send old and new ItemCategories for triggering Audit Logs based on data
     *
     * @public
     */
    public void logUpsert(Action action, ItemCategories newData) {
        LoggerHelper.logMethodEntry(logger, ITEM_CATEGORY_HANDLER, "logUpsert");
        auditLogHelper.logConfigChange(ItemCategories_.CDS_NAME, action, newData);
        LoggerHelper.logMethodExit(logger, ITEM_CATEGORY_HANDLER, "logUpsert");
    }

    /**
     * To set old data before to compare with new data for triggering audit togs
     *
     * @public
     */
    public void setOldAuditData(ItemCategories itemCategory) {
        LoggerHelper.logMethodEntry(logger, ITEM_CATEGORY_HANDLER, "setOldAuditData");
        ItemCategories oldData = service.getItemCategoryDetails(itemCategory.getId());
        auditLogHelper.setOldData(oldData);
        LoggerHelper.logMethodExit(logger, ITEM_CATEGORY_HANDLER, "setOldAuditData");
    }
}

