package com.sap.ic.cmh.masterdata.purchaseorganization.handler;

import cds.gen.masterdataservice.CompanyCodes;
import cds.gen.masterdataservice.PurchaseOrganizations;
import cds.gen.masterdataservice.PurchasingGroups;
import com.sap.cds.Result;
import com.sap.cds.Struct;
import com.sap.cds.ql.cqn.CqnInsert;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.cds.CdsCreateEventContext;
import com.sap.cds.services.cds.CdsUpdateEventContext;
import com.sap.cds.services.messages.Messages;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.ic.cmh.masterdata.companycode.service.CompanyCodeService;
import com.sap.ic.cmh.masterdata.purchaseorganization.service.PurchaseOrganizationService;
import com.sap.ic.cmh.masterdata.purchaseorganization.validation.PurchaseOrganizationValidator;
import com.sap.ic.cmh.masterdata.purchasinggroup.service.PurchasingGroupService;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.*;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.when;

public class PurchaseOrganizationHandlerTest {
    @InjectMocks
    @Autowired
    private PurchaseOrganizationHandler purchaseOrganizationHandler;

    @Mock
    protected PersistenceService mockDb;

    @Mock
    CompanyCodeService companyCodeService;
    @Mock
    PurchaseOrganizationValidator validator;

    @Mock
    private Messages messages;
    @Mock
    PurchaseOrganizationService purchaseOrganizationService;

    @Mock
    private PurchaseOrganizationValidator purchaseOrganizationValidator;

    @Mock
    CdsCreateEventContext createContextMock;

    @Mock
    CqnInsert cqnInsert;
    @Mock
    CdsUpdateEventContext updateEventContext;
    @Mock
    Result result;
    @Mock
    PurchasingGroupService service;
    private PurchaseOrganizations purchaseOrganizations;

    private CompanyCodes companyCodeItem;

    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
        purchaseOrganizations = Struct.create(PurchaseOrganizations.class);

        purchaseOrganizations.setPurchaseOrganization("SO10");
        purchaseOrganizations.setPurchaseOrganizationName("Capgemini Auto sap");
        purchaseOrganizations.setCompanyCode("BP01");

        companyCodeItem = Struct.create(CompanyCodes.class);
        companyCodeItem.setId(UUID.randomUUID().toString());
        companyCodeItem.setCompanyCode(purchaseOrganizations.getCompanyCode());
        when(createContextMock.getCqn()).thenReturn(cqnInsert);
        List<Map<String, Object>> entries = new ArrayList<>();
        when(cqnInsert.entries()).thenReturn(entries);
    }
    @Test
    public void testOnPlantsCreate(){
        Optional<PurchaseOrganizations> opt = Optional.of(purchaseOrganizations);
        Mockito.when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        Mockito.when(result.first(PurchaseOrganizations.class)).thenReturn(opt);
        when(purchaseOrganizationService.fetchPurchaseOrganization(purchaseOrganizations.getPurchaseOrganization())).thenReturn(purchaseOrganizations);

        purchaseOrganizationHandler.onPurchaseOrganizationsCreate(createContextMock,purchaseOrganizations);
    }
    @Test
    public void testCreatePurchaseOrganization()  {
        Optional<PurchaseOrganizations> emptyOpt = Optional.empty();
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.first(PurchaseOrganizations.class)).thenReturn(emptyOpt);
        doNothing().when(validator).checkInputsSanitized(purchaseOrganizations);
        purchaseOrganizationHandler.beforePurchaseOrganizationsCreate(createContextMock, purchaseOrganizations);
    }

    @Test
    public void testUpdatePurchasingGroups()  {
        Optional<PurchaseOrganizations> emptyOpt = Optional.of(purchaseOrganizations);
        purchaseOrganizationHandler.beforePurchaseOrganizationsUpdate(updateEventContext, purchaseOrganizations);
    }


}
