package com.sap.ic.cmh.configuration.action.handler;

import cds.gen.configurationservice.DeactivateItemCategoryContext;
import cds.gen.configurationservice.ItemCategories;
import com.sap.cds.Result;
import com.sap.cds.Struct;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.cds.CdsService;
import com.sap.cds.services.messages.Message;
import com.sap.cds.services.messages.Messages;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.ic.cmh.auditlog.AuditLogHelper;
import com.sap.ic.cmh.configuration.handler.ItemCategoryHandler;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.when;

public class DeactivateItemCategoryHandlerTest {
    @InjectMocks
    DeactivateItemCategoryHandler handler;

    @Mock
    DeactivateItemCategoryContext context;

    @Mock
    CqnSelect cqnSelect;

    @Mock
    private CdsService cdsService;

    @Mock
    AuditLogHelper<ItemCategories> auditLogHelper;

    @Mock
    ItemCategoryHandler itemCategoryHandler;

    private
    ItemCategories itemCategory;
    @Mock
    Result result;

    @Mock
    protected PersistenceService db;

    @Mock
    Messages messages;

    @Mock
    private Message msg;

    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
        itemCategory = Struct.create(ItemCategories.class);

    }

    @Test
    public void deactivateItemCategoryTest() {
        when(context.getCqn()).thenReturn(cqnSelect);
        when(context.getService()).thenReturn(cdsService);
        when(cdsService.run(any(CqnSelect.class))).thenReturn(result);
        when(result.single(ItemCategories.class)).thenReturn(itemCategory);
        when(messages.success(anyString())).thenReturn(msg);
        itemCategory.setIsActiveEntity(true);
        itemCategory.setIsActive(false);
        handler.deactivateItemCategory(context);
    }

    @Test
    public void deactivateItemCategoryIsActiveTest() {
        when(context.getCqn()).thenReturn(cqnSelect);
        when(context.getService()).thenReturn(cdsService);
        when(cdsService.run(any(CqnSelect.class))).thenReturn(result);
        when(result.single(ItemCategories.class)).thenReturn(itemCategory);
        when(messages.success(anyString())).thenReturn(msg);
        itemCategory.setIsActiveEntity(true);
        itemCategory.setIsActive(true);
        handler.deactivateItemCategory(context);
    }

    @Test
    public void deactivateItemCategoryIsInActiveTest() {
        when(context.getCqn()).thenReturn(cqnSelect);
        when(context.getService()).thenReturn(cdsService);
        when(cdsService.run(any(CqnSelect.class))).thenReturn(result);
        when(result.single(ItemCategories.class)).thenReturn(itemCategory);
        when(messages.success(anyString())).thenReturn(msg);
        itemCategory.setIsActiveEntity(false);
        handler.deactivateItemCategory(context);
    }

    @Test
    public void afterDeactivateItemCategoryTest() {
        when(context.getCqn()).thenReturn(cqnSelect);
        when(context.getService()).thenReturn(cdsService);
        when(cdsService.run(any(CqnSelect.class))).thenReturn(result);
        when(result.single(ItemCategories.class)).thenReturn(itemCategory);
        handler.afterDeactivateItemCategory(context);
    }
}
