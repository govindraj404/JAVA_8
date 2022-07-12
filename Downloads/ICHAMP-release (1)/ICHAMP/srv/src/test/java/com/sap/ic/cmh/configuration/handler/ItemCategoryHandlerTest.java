package com.sap.ic.cmh.configuration.handler;

import cds.gen.configurationservice.ItemCategories;
import com.sap.cds.Result;
import com.sap.cds.Row;
import com.sap.cds.Struct;
import com.sap.cds.services.auditlog.Action;
import com.sap.cds.services.draft.DraftNewEventContext;
import com.sap.cds.services.messages.Message;
import com.sap.cds.services.messages.Messages;
import com.sap.ic.cmh.auditlog.AuditLogHelper;
import com.sap.ic.cmh.configuration.service.ItemCategoryService;
import com.sap.ic.cmh.configuration.validations.ItemCategoryValidation;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.stream.Stream;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.when;

public class ItemCategoryHandlerTest {

    @InjectMocks
    ItemCategoryHandler handler;

    @Mock
    ItemCategoryValidation validation;

    @Mock
    ItemCategoryService service;

    @Mock
    DraftNewEventContext context;

    @Mock
    Messages messages;

    @Mock
    private Message msg;

    @Mock
    Result result;

    @Mock
    private AuditLogHelper<ItemCategories> auditLogHelper;

    ItemCategories itemCategory;

    private Row row;

    private Optional<Row> opt;

    @Before
    public void setBefore() {
        MockitoAnnotations.openMocks(this);
        itemCategory = Struct.create(ItemCategories.class);
        row = Struct.create(Row.class);
        row.put("code", "CODE100");
        row.put("identifier", "11");
        opt = Optional.of(row);
    }

    @Test
    public void testBeforeCreateItemCategoryDraft() {
        handler.beforeCreateItemCategoryDraft(context, itemCategory);
    }

    @Test
    public void testBeforeCreateItemCategoryBooleanCheck() {
        itemCategory.setMaterialEnteredManually(false);
        itemCategory.setReceivedQuantityEditable(false);
        itemCategory.setReturnQuantityEditable(false);
        itemCategory.setIndividualComplaint(false);
        itemCategory.setExternalReferenceMandatory(false);
        itemCategory.setExternalReferenceCheckedForDuplication(false);
        itemCategory.setCreditDebitAmountEnteredManually(false);
        handler.beforeCreateItemCategoryDraft(context, itemCategory);
    }

    @Test
    public void testBeforeCreateItemCategory() {
        when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
        when(msg.target(any(String.class))).thenReturn(msg);
        doNothing().when(validation).validateItemCategories(itemCategory);
        handler.beforeCreateItemCategory(itemCategory);
    }

    @Test
    public void testOnCreateItemCategoryWithEmptyIdentifier() {
        row = Struct.create(Row.class);
        row.put("code", "CODE100");
        opt = Optional.of(row);
        when(service.getItemCategories()).thenReturn(result);
        when(result.first()).thenReturn(opt);
        handler.onCreateItemCategory(itemCategory);
    }

    @Test
    public void testOnCreateItemCategory() {
        when(service.getItemCategories()).thenReturn(result);
        when(result.first()).thenReturn(opt);
        handler.onCreateItemCategory(itemCategory);
    }

    @Test
    public void testOnCreateItemCategoryWithEmptyData() {
        opt = Optional.empty();
        when(service.getItemCategories()).thenReturn(result);
        when(result.first()).thenReturn(opt);
        handler.onCreateItemCategory(itemCategory);
    }


    @Test
    public void testBeforeUpdateItemCategory() {
        doNothing().when(validation).validateItemCategories(itemCategory);
        handler.beforeUpdateItemCategory(itemCategory);
    }

    @Test
    public void testAfterReadItemCategoryBooleanTrueCheck() {
        itemCategory.setMaterialEnteredManually(true);
        itemCategory.setCreditDebitAmountEnteredManually(true);
        List<ItemCategories> list = new ArrayList<>();
        list.add(itemCategory);
        Stream<ItemCategories> streamItemCategories = list.stream();
        handler.afterReadItemCategory(streamItemCategories);
    }

    @Test
    public void testAfterReadItemCategoryBooleanFalseCheck() {
        itemCategory.setMaterialEnteredManually(false);
        itemCategory.setCreditDebitAmountEnteredManually(false);
        List<ItemCategories> list = new ArrayList<>();
        list.add(itemCategory);
        Stream<ItemCategories> streamItemCategories = list.stream();
        handler.afterReadItemCategory(streamItemCategories);
    }

    @Test
    public void testAfterReadItemCategoryNullCheck() {
        itemCategory.setMaterialEnteredManually(null);
        itemCategory.setCreditDebitAmountEnteredManually(null);
        List<ItemCategories> list = new ArrayList<>();
        list.add(itemCategory);
        Stream<ItemCategories> streamItemCategories = list.stream();
        handler.afterReadItemCategory(streamItemCategories);
    }

    @Test
    public void testAfterReadItemCategoryBlankCheck() {
        itemCategory.setReferenceDocumentCategoryCode("");
        List<ItemCategories> list = new ArrayList<>();
        list.add(itemCategory);
        Stream<ItemCategories> streamItemCategories = list.stream();
        handler.afterReadItemCategory(streamItemCategories);
    }

    @Test
    public void testAfterReadItemCategoryCheck() {
        itemCategory.setReferenceDocumentCategoryCode("code");
        List<ItemCategories> list = new ArrayList<>();
        itemCategory.setIsActiveEntity(true);
        itemCategory.setHasDraftEntity(true);
        itemCategory.setIsActive(true);
        list.add(itemCategory);
        Stream<ItemCategories> streamItemCategories = list.stream();
        handler.afterReadItemCategory(streamItemCategories);
    }

    @Test
    public void testBeforePatchItemCategoryBooleanFalseCheck() {
        itemCategory.setMaterialEnteredManually(false);
        itemCategory.setCreditDebitAmountEnteredManually(false);
        itemCategory.setReferenceDocumentCategoryCode("code");
        handler.beforePatchItemCategory(itemCategory);
    }

    @Test
    public void testBeforePatchItemCategoryNullCheck() {
        itemCategory.setMaterialEnteredManually(null);
        itemCategory.setCreditDebitAmountEnteredManually(null);
        itemCategory.setReferenceDocumentCategoryCode("code");
        handler.beforePatchItemCategory(itemCategory);
    }

    @Test
    public void testBeforePatchItemCategoryBooleanTrueCheck() {
        itemCategory.setMaterialEnteredManually(true);
        itemCategory.setCreditDebitAmountEnteredManually(true);
        itemCategory.setReferenceDocumentCategoryCode("code");
        handler.beforePatchItemCategory(itemCategory);
    }

    @Test
    public void testBeforePatchItemCategoryBlankCheck() {
        itemCategory.setMaterialEnteredManually(null);
        itemCategory.setReferenceDocumentCategoryCode("");
        handler.beforePatchItemCategory(itemCategory);
    }

    @Test
    public void afterItemCategoryIsActiveNullTest() {
        List<ItemCategories> list = new ArrayList<>();
        itemCategory.setIsActiveEntity(true);
        itemCategory.setHasDraftEntity(true);
        list.add(itemCategory);
        handler.afterReadItemCategory(list.stream());
    }

    @Test
    public void afterItemCategoryHasDraftNullTest() {
        List<ItemCategories> list = new ArrayList<>();
        itemCategory.setIsActiveEntity(true);
        itemCategory.setIsActive(true);
        list.add(itemCategory);
        handler.afterReadItemCategory(list.stream());
    }

    @Test
    public void afterItemCategoryHasDraftFalseTest() {
        List<ItemCategories> list = new ArrayList<>();
        itemCategory.setIsActiveEntity(true);
        itemCategory.setHasDraftEntity(false);
        itemCategory.setIsActive(true);
        list.add(itemCategory);
        handler.afterReadItemCategory(list.stream());
    }

    @Test
    public void afterItemCategoryIsActiveFalseTest() {
        List<ItemCategories> list = new ArrayList<>();
        itemCategory.setIsActiveEntity(false);
        itemCategory.setHasDraftEntity(true);
        itemCategory.setIsActive(true);
        list.add(itemCategory);
        handler.afterReadItemCategory(list.stream());
    }

    @Test
    public void afterItemCategoryIsActiveTrueTest() {
        List<ItemCategories> list = new ArrayList<>();
        itemCategory.setIsActiveEntity(true);
        itemCategory.setHasDraftEntity(false);
        itemCategory.setIsActive(false);
        list.add(itemCategory);
        handler.afterReadItemCategory(list.stream());
    }

    @Test
    public void afterUpdateItemCategoryTest() {

        handler.afterUpdateItemCategory(itemCategory);
    }

    @Test
    public void afterCreateItemCategoryTest() {
        handler.afterCreateItemCategory(itemCategory);
    }

    @Test
    public void logUpsertTest() {
        handler.logUpsert(Action.UPDATE, itemCategory);
    }

    @Test
    public void setOldDataTest() {
        handler.setOldAuditData(itemCategory);
    }
}
