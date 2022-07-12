package com.sap.ic.cmh.configuration.action.handler;

import cds.gen.configurationservice.CopyItemCategoriesContext;
import cds.gen.configurationservice.ItemCategories;
import com.sap.cds.Result;
import com.sap.cds.Struct;
import com.sap.cds.ql.cqn.CqnInsert;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.cds.CdsService;
import com.sap.cds.services.draft.DraftService;
import com.sap.cds.services.messages.Messages;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

public class CopyItemCategoryHandlerTest {

    @InjectMocks
    CopyItemCategoryHandler handler;

    @Mock
    DraftService draftService;

    @Mock
    Messages messages;

    @Mock
    Result result;

    @Mock
    CopyItemCategoriesContext context;

    @Mock
    CdsService cdsService;

    @Mock
    CqnSelect cqnSelect;

    ItemCategories itemCategories;

    @Before
    public void setBefore() {
        MockitoAnnotations.openMocks(this);
        itemCategories = Struct.create(ItemCategories.class);

    }

    @Test
    public void testCopyItemCategory() {
        when(context.getCqn()).thenReturn(cqnSelect);
        when(context.getService()).thenReturn(cdsService);
        when(cdsService.run(any(CqnSelect.class))).thenReturn(result);
        when(result.single(ItemCategories.class)).thenReturn(itemCategories);
        when(draftService.newDraft(any(CqnInsert.class))).thenReturn(result);
        handler.copyItemCategory(context);

    }
}
