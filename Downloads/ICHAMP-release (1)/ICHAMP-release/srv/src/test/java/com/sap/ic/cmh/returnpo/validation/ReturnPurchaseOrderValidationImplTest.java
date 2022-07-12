package com.sap.ic.cmh.returnpo.validation;

import cds.gen.configurationservice.BusinessObjectConfigurations;
import cds.gen.masterdataservice.*;
import cds.gen.qualitynotificationservice.QualityNotifications;
import cds.gen.returnpurchaseorderservice.ReturnPurchaseOrders;
import com.sap.cds.Result;
import com.sap.cds.Row;
import com.sap.cds.Struct;
import com.sap.cds.ql.cqn.CqnInsert;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.cds.CdsCreateEventContext;
import com.sap.cds.services.messages.Message;
import com.sap.cds.services.messages.Messages;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.ic.cmh.businessobjects.service.BusinessObjectService;
import com.sap.ic.cmh.configuration.model.MasterData;
import com.sap.ic.cmh.configuration.persistency.ConfigurationDao;
import com.sap.ic.cmh.configuration.validations.MasterDataValidation;

import com.sap.ic.cmh.gen.MessageKeys;
import com.sap.ic.cmh.returnpo.service.ReturnPurchaseOrderService;
import com.sap.ic.cmh.utils.SecurityValidator;
import com.sap.ic.cmh.utils.datavalidation.DataValidatorImpl;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

public class ReturnPurchaseOrderValidationImplTest {
    @InjectMocks
    ReturnPurchaseOrderValidationImpl validator;
    @Mock
    private DataValidatorImpl dataValidator;
    @Mock
    private Messages messages;
    @Mock
    private Message msg;
    @Mock
    private Runnable run;

    @Mock
    ReturnPurchaseOrderService returnPurchaseOrderService;
    @Mock
    MasterDataValidation masterDataValidation;
    @Mock
    Result result;
    @Mock
    CdsCreateEventContext createContextMock;
    @Mock
    CqnInsert cqnInsert;
    @Mock
    BusinessObjectService businessObjectService;
    @Mock
    public PersistenceService db;
    @Mock
    ConfigurationDao configurationDao;

    @Mock
    SecurityValidator securityValidator;
    private   ReturnPurchaseOrders returnPurchaseOrders;
    private   ReturnPurchaseOrders returnPurchaseOrdersdb;

    private  CompanyCodes companycode;
    @Mock
    private MasterData masterData;
    private PurchaseOrganizations purchaseOrganizations;
    private BusinessPartners businessPartners;
    private BusinessObjectConfigurations businessObjectConfigurations;
    private MaterialMasterGeneralDatas materialMasterGeneralDatas;
    private Plants plants;
    QualityNotifications qualityNotifications;
    PurchasingGroups purchasingGroups;
   @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
        purchasingGroups = Struct.create(PurchasingGroups.class);
        purchasingGroups.setCode("202");
        purchasingGroups.setDescription("test");
        qualityNotifications = Struct.create(QualityNotifications.class);
        qualityNotifications.setMaterialId("test");
        qualityNotifications.setPlantId("test");
        qualityNotifications.setSupplierId("test");
        qualityNotifications.setIdentifier("Fp01");
        businessObjectConfigurations = Struct.create(BusinessObjectConfigurations.class);
        businessPartners = Struct.create(BusinessPartners.class);
        businessPartners.setBusinessPartnerNumber("202");
        businessObjectConfigurations.setBusinessObjectTypeCode("202");
        materialMasterGeneralDatas = Struct.create(MaterialMasterGeneralDatas.class);
        materialMasterGeneralDatas.setMaterialCode("44");
        plants = Struct.create(Plants.class);
        plants.setPlant("Fp01");
        masterData.setPurchaseOrg(purchaseOrganizations);
        masterData.setSupplier(businessPartners);
        masterData.setPlants(plants);

        returnPurchaseOrders = Struct.create(ReturnPurchaseOrders.class);
        companycode = Struct.create(CompanyCodes.class);
        companycode.setCompanyCode("12");
        companycode.setId("201");
        returnPurchaseOrders.setCompanyId("201");
        returnPurchaseOrders.setComplaintId("101");
        returnPurchaseOrders.setContactPerson(businessPartners);
        returnPurchaseOrders.setReturnPurchaseType("PO");
        returnPurchaseOrders.setIdentifier("test");
        returnPurchaseOrders.setSupplierId("201");
        returnPurchaseOrders.setPersonResponsibleId("DFGTB");
        returnPurchaseOrders.setPurchasingGroupCode("001");


        returnPurchaseOrdersdb = Struct.create(ReturnPurchaseOrders.class);

        returnPurchaseOrdersdb.setCompanyId("201");
        returnPurchaseOrdersdb.setComplaintId("101");
        //returnPurchaseOrdersdb.setContactPerson("sap");
        returnPurchaseOrdersdb.setReturnPurchaseType("PO");
        returnPurchaseOrdersdb.setIdentifier("test");
        returnPurchaseOrdersdb.setSupplierId("201");
        returnPurchaseOrdersdb.setReasonCode("FARED");
        returnPurchaseOrdersdb.setPersonResponsibleId("DAFGTB");


    }

    @Test
    public void validateMandatoryFieldsTest() {
        BusinessPartners businessPartners=Struct.create(BusinessPartners.class);
        Optional<BusinessPartners> emptyOpt = Optional.of(businessPartners);
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        when(result.first(BusinessPartners.class)).thenReturn(emptyOpt);
        Mockito.when(configurationDao.getSupplierData(any(String.class))).thenReturn(result);
        Mockito.when(messages.error(any(String.class),any(Object[].class))).thenReturn(msg);
        Mockito.when(msg.target(any(String.class))).thenReturn(msg);
        validator.validateMandatoryFields(returnPurchaseOrders);
    }

    @Test
    public void testValidateMandatoryFielDReasonCodeIsNotBlank() {
        returnPurchaseOrders.setReasonCode("dfghtyy");
        BusinessPartners businessPartners=Struct.create(BusinessPartners.class);
        Optional<BusinessPartners> emptyOpt = Optional.of(businessPartners);
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        when(result.first(BusinessPartners.class)).thenReturn(emptyOpt);
        Mockito.when(configurationDao.getSupplierData(any(String.class))).thenReturn(result);
        Mockito.when(messages.error(any(String.class),any(Object[].class))).thenReturn(msg);
        Mockito.when(msg.target(any(String.class))).thenReturn(msg);
        validator.validateMandatoryFields(returnPurchaseOrders);
    }

    @Test
    public void testValidateMandatoryFieldItemNumberIsNotBlank() {
        returnPurchaseOrders.setItemNumber("dfghtyy");
        BusinessPartners businessPartners=Struct.create(BusinessPartners.class);
        Optional<BusinessPartners> emptyOpt = Optional.of(businessPartners);
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        when(result.first(BusinessPartners.class)).thenReturn(emptyOpt);
        Mockito.when(configurationDao.getSupplierData(any(String.class))).thenReturn(result);
        Mockito.when(messages.error(any(String.class),any(Object[].class))).thenReturn(msg);
        Mockito.when(msg.target(any(String.class))).thenReturn(msg);
        validator.validateMandatoryFields(returnPurchaseOrders);
    }


    @Test
    public void validateMandatoryIsMarkedForDeletioTest() {
        BusinessPartners businessPartners=Struct.create(BusinessPartners.class);
        businessPartners.setIsMarkedForDeletion(true);
        businessPartners.setId("202");
        List<BusinessPartners> businessPartnersList=new ArrayList<>();
        businessPartnersList.add(businessPartners);
        Optional<BusinessPartners> emptyOpt = Optional.of(businessPartners);
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        Row row=Struct.create(Row.class);
        row.put("isMarkedForDeletion","test");
        when(result.listOf(BusinessPartners.class)).thenReturn(businessPartnersList);
        when(result.first(BusinessPartners.class)).thenReturn(emptyOpt);
        Mockito.when(configurationDao.getSupplierData(any(String.class))).thenReturn(result);
        when(result.first()).thenReturn(Optional.of(row));
        Mockito.when(messages.error(any(String.class),any(Object[].class))).thenReturn(msg);
        Mockito.when(msg.target(any(String.class))).thenReturn(msg);
        validator.validateMandatoryFields(returnPurchaseOrders);
    }

    @Test
    public void validateMandatorygetBusinessPartnerTypeTest() {
        BusinessPartners businessPartners=Struct.create(BusinessPartners.class);
        businessPartners.setBusinessPartnerType("true");
        businessPartners.setId("202");
        List<BusinessPartners> businessPartnersList=new ArrayList<>();
        businessPartnersList.add(businessPartners);
        Optional<BusinessPartners> emptyOpt = Optional.of(businessPartners);
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        Row row=Struct.create(Row.class);
        row.put("businessPartnerType","test");
        when(result.listOf(BusinessPartners.class)).thenReturn(businessPartnersList);
        when(result.first(BusinessPartners.class)).thenReturn(emptyOpt);
        Mockito.when(configurationDao.getSupplierData(any(String.class))).thenReturn(result);
        when(result.first()).thenReturn(Optional.of(row));
        Mockito.when(messages.error(any(String.class),any(Object[].class))).thenReturn(msg);
        Mockito.when(msg.target(any(String.class))).thenReturn(msg);
        validator.validateMandatoryFields(returnPurchaseOrders);
    }
    @Test(expected = Exception.class)
    public void validateMandatoryFieldsNullTest() {
        Mockito.when(messages.error(any(String.class),any(Object[].class))).thenReturn(msg);
        Mockito.when(msg.target(any(String.class))).thenReturn(msg);
        validator.validateMandatoryFields(null);
    }

    @Test
    public void validateTypeTest() {
        validator.validateType("test");
    }

    @Test
    public void validateTypeNullTest() {
        Mockito.when(messages.error(any(String.class),any(Object[].class))).thenReturn(msg);
        Mockito.when(msg.target(any(String.class))).thenReturn(msg);
        validator.validateType("");
    }

    @Test
    public void validatePurchasingGroupIfExistTest() {
        Optional<PurchasingGroups> emptyOpt = Optional.empty();
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        when(result.first(PurchasingGroups.class)).thenReturn(emptyOpt);
        Mockito.when(messages.error(any(String.class),any(Object[].class))).thenReturn(msg);
        Mockito.when(msg.target(any(String.class))).thenReturn(msg);
        validator.validatePurchasingGroupIfExist("test");
    }
    @Test
    public void validateReasonIfExistTest(){
        Optional<PurchasingGroups> emptyOpt = Optional.empty();
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        when(result.first(PurchasingGroups.class)).thenReturn(emptyOpt);
        Mockito.when(messages.error(any(String.class),any(Object[].class))).thenReturn(msg);
        Mockito.when(msg.target(any(String.class))).thenReturn(msg);
        validator.validateReasonIfExist("test");
    }
    @Test
    public void validateFieldControlReturnPOTest(){
        Optional<PurchasingGroups> emptyOpt = Optional.empty();
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        when(result.first(PurchasingGroups.class)).thenReturn(emptyOpt);
        returnPurchaseOrders.setStatusCode("200");
        Mockito.when(returnPurchaseOrderService.getReturnPurchaseOrderDetails(any(String.class))).thenReturn(returnPurchaseOrders);
        Mockito.when(messages.error(any(String.class),any(Object[].class))).thenReturn(msg);
        Mockito.when(msg.target(any(String.class))).thenReturn(msg);
        validator.validateFieldControlReturnPO(returnPurchaseOrders);

    }
    @Test
    public void validateFieldControlReturnPONullTest(){
        Optional<PurchasingGroups> emptyOpt = Optional.empty();
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        when(result.first(PurchasingGroups.class)).thenReturn(emptyOpt);
        returnPurchaseOrders.setStatusCode("200");
        Mockito.when(returnPurchaseOrderService.getReturnPurchaseOrderDetails(any(String.class))).thenReturn(null);
        Mockito.when(messages.error(any(String.class),any(Object[].class))).thenReturn(msg);
        Mockito.when(msg.target(any(String.class))).thenReturn(msg);
        validator.validateFieldControlReturnPO(returnPurchaseOrders);

    }

    @Test
    public void validateFieldControlReturnPOEmptyTest(){
        Optional<PurchasingGroups> emptyOpt = Optional.empty();
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        when(result.first(PurchasingGroups.class)).thenReturn(emptyOpt);
        returnPurchaseOrders.setStatusCode("NEW");
        Mockito.when(returnPurchaseOrderService.getReturnPurchaseOrderDetails(any(String.class))).thenReturn(null);
        Mockito.when(messages.error(any(String.class),any(Object[].class))).thenReturn(msg);
        Mockito.when(msg.target(any(String.class))).thenReturn(msg);
        validator.validateFieldControlReturnPO(returnPurchaseOrders);

    }

    @Test
    public void validateFieldControlReturnPONotCloseTest(){
        Optional<PurchasingGroups> emptyOpt = Optional.empty();
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        when(result.first(PurchasingGroups.class)).thenReturn(emptyOpt);
        returnPurchaseOrders.setStatusCode("NEW");
        Mockito.when(returnPurchaseOrderService.getReturnPurchaseOrderDetails(any(String.class))).thenReturn(returnPurchaseOrders);
        Mockito.when(messages.error(any(String.class),any(Object[].class))).thenReturn(msg);
        Mockito.when(msg.target(any(String.class))).thenReturn(msg);
        validator.validateFieldControlReturnPO(returnPurchaseOrders);

    }
    @Test
    public void validateFieldControlReturnPOCloseTest(){
        Optional<PurchasingGroups> emptyOpt = Optional.empty();
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        when(result.first(PurchasingGroups.class)).thenReturn(emptyOpt);
        returnPurchaseOrders.setStatusCode("CLSD");
        Mockito.when(returnPurchaseOrderService.getReturnPurchaseOrderDetails(any(String.class))).thenReturn(null);
        Mockito.when(messages.error(any(String.class),any(Object[].class))).thenReturn(msg);
        Mockito.when(msg.target(any(String.class))).thenReturn(msg);
        validator.validateFieldControlReturnPO(returnPurchaseOrders);

    }

    @Test
    public void validateifBOIsRelevantTest() {
        when(businessObjectService.checkIfBOIsRelevant(any(String.class),any(String.class))).thenReturn(true);
        Optional<PurchasingGroups> emptyOpt = Optional.empty();
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        when(result.first(PurchasingGroups.class)).thenReturn(emptyOpt);
        Mockito.when(messages.error(any(String.class),any(Object[].class))).thenReturn(msg);
        Mockito.when(msg.target(any(String.class))).thenReturn(msg);
        validator.validateifBOIsRelevant("8","4");
    }

    @Test
    public void validateifBOIsRelevantfalseTest() {
        when(businessObjectService.checkIfBOIsRelevant(any(String.class),any(String.class))).thenReturn(false);
        Optional<PurchasingGroups> emptyOpt = Optional.empty();
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        when(result.first(PurchasingGroups.class)).thenReturn(emptyOpt);
        Mockito.when(messages.error(any(String.class),any(Object[].class))).thenReturn(msg);
        Mockito.when(msg.target(any(String.class))).thenReturn(msg);
        validator.validateifBOIsRelevant("8","4");
    }
    @Test
    public void validateIfReturnPurchaseOrderExistsForComplaintTest() {
        when(returnPurchaseOrderService.getReturnPurchaseOrderDetailsBasedOnComplaintId(any(String.class))).thenReturn(returnPurchaseOrders);
        Optional<PurchasingGroups> emptyOpt = Optional.empty();
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        when(result.first(PurchasingGroups.class)).thenReturn(emptyOpt);
        when(messages.error(any(String.class),any(Object[].class))).thenReturn(msg);
        when(msg.target(any(String.class))).thenReturn(msg);
        validator.validateIfReturnPurchaseOrderExistsForComplaint("8");
    }

    @Test
    public void validateIfReturnPurchaseOrderExistsTest() {
        when(returnPurchaseOrderService.getReturnPurchaseOrderDetails(any(String.class))).thenReturn(null);
        Optional<PurchasingGroups> emptyOpt = Optional.empty();
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        when(result.first(PurchasingGroups.class)).thenReturn(emptyOpt);
        Mockito.when(messages.error(any(String.class),any(Object[].class))).thenReturn(msg);
        Mockito.when(msg.target(any(String.class))).thenReturn(msg);
        validator.validateIfReturnPurchaseOrderExists("8");
    }
    @Test
    public void validateFieldControlReturnPOElseTest() {
        returnPurchaseOrders.setStatusCode("RPOCLSD");
        when(returnPurchaseOrderService.getReturnPurchaseOrderDetails(any(String.class))).thenReturn(returnPurchaseOrders);
        Optional<PurchasingGroups> emptyOpt = Optional.empty();
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        when(result.first(PurchasingGroups.class)).thenReturn(emptyOpt);
        Mockito.when(messages.error(any(String.class),any(Object[].class))).thenReturn(msg);
        Mockito.when(msg.target(any(String.class))).thenReturn(msg);
        validator.validateFieldControlReturnPO((returnPurchaseOrders));
    }

    @Test
    public void validateFieldControlReturnPOHasValue() {
        returnPurchaseOrders.setStatusCode("RPOCLSD");
        when(returnPurchaseOrderService
                .getReturnPurchaseOrderDetails(returnPurchaseOrders.getId())).thenReturn(returnPurchaseOrders);
        when(returnPurchaseOrderService.getReturnPurchaseOrderDetails(any(String.class))).thenReturn(returnPurchaseOrders);
        Optional<PurchasingGroups> emptyOpt = Optional.empty();
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        when(result.first(PurchasingGroups.class)).thenReturn(emptyOpt);
        Mockito.when(messages.error(any(String.class),any(Object[].class))).thenReturn(msg);
        Mockito.when(msg.target(any(String.class))).thenReturn(msg);
        validator.validateFieldControlReturnPO((returnPurchaseOrders));
    }

    @Test
    public void validateFieldControlReturnPONEW() {
        returnPurchaseOrders.setStatusCode("NEW");
        when(returnPurchaseOrderService
                .getReturnPurchaseOrderDetails(returnPurchaseOrders.getId())).thenReturn(returnPurchaseOrders);
        when(returnPurchaseOrderService.getReturnPurchaseOrderDetails(any(String.class))).thenReturn(returnPurchaseOrders);
        Optional<PurchasingGroups> emptyOpt = Optional.empty();
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        when(result.first(PurchasingGroups.class)).thenReturn(emptyOpt);
        Mockito.when(messages.error(any(String.class),any(Object[].class))).thenReturn(msg);
        Mockito.when(msg.target(any(String.class))).thenReturn(msg);
        validator.validateFieldControlReturnPO((returnPurchaseOrders));
    }
    @Test
    public void validateFieldControlReturnPONotNewWithValues() {
        returnPurchaseOrders.setStatusCode("RGI");
        returnPurchaseOrders.setReasonCode("FRED");
        when(returnPurchaseOrderService.getReturnPurchaseOrderDetails(returnPurchaseOrders.getId())).thenReturn(returnPurchaseOrdersdb);
        Optional<PurchasingGroups> emptyOpt = Optional.empty();
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        when(result.first(PurchasingGroups.class)).thenReturn(emptyOpt);
        Mockito.when(messages.error(any(String.class),any(Object[].class))).thenReturn(msg);
        when(messages.error(MessageKeys.RETURN_ORDER_REASON_NOT_EDITABLE)).thenReturn(msg);
        Mockito.when(msg.target(any(String.class))).thenReturn(msg);
        validator.validateFieldControlReturnPO((returnPurchaseOrders));
    }

    @Test
    public void validateFieldControlReturnPONEWWithValuesNullTest() {
        returnPurchaseOrders.setStatusCode("NEW");
        returnPurchaseOrders.setReasonCode("FARED");
        when(returnPurchaseOrderService.getReturnPurchaseOrderDetails(returnPurchaseOrders.getId())).thenReturn(returnPurchaseOrdersdb);
        Optional<PurchasingGroups> emptyOpt = Optional.empty();
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        when(result.first(PurchasingGroups.class)).thenReturn(emptyOpt);
        Mockito.when(messages.error(any(String.class),any(Object[].class))).thenReturn(msg);
        when(messages.error(MessageKeys.RETURN_ORDER_REASON_NOT_EDITABLE)).thenReturn(msg);
        Mockito.when(msg.target(any(String.class))).thenReturn(msg);
        validator.validateFieldControlReturnPO((returnPurchaseOrders));
    }

    @Test
    public void validateFieldControlReturnPONEWWithValues1() {
        returnPurchaseOrders.setStatusCode("GR");
        returnPurchaseOrders.setId("1234");
        returnPurchaseOrdersdb.setId("1234");
        when(returnPurchaseOrderService.getReturnPurchaseOrderDetails(any(String.class))).thenReturn(returnPurchaseOrdersdb);
        Optional<PurchasingGroups> emptyOpt = Optional.empty();
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        when(result.first(PurchasingGroups.class)).thenReturn(emptyOpt);
        Mockito.when(messages.error(any(String.class),any(Object[].class))).thenReturn(msg);
        when(messages.error(MessageKeys.PERSON_RESPONISBLE_NOT_EDITABLE)).thenReturn(msg);
        Mockito.when(msg.target(any(String.class))).thenReturn(msg);
        validator.validateFieldControlReturnPO(returnPurchaseOrders);
    }

    @Test
    public void validateFieldControlReturnPONEWWithValuesNull() {
        returnPurchaseOrders.setStatusCode("NEW");
        returnPurchaseOrders.setId("1234");
        returnPurchaseOrders.setPersonResponsibleId("DAFGTB");
        returnPurchaseOrdersdb.setId("1234");
        when(returnPurchaseOrderService.getReturnPurchaseOrderDetails(any(String.class))).thenReturn(returnPurchaseOrdersdb);
        Optional<PurchasingGroups> emptyOpt = Optional.empty();
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        when(result.first(PurchasingGroups.class)).thenReturn(emptyOpt);
        Mockito.when(messages.error(any(String.class),any(Object[].class))).thenReturn(msg);
        Mockito.when(msg.target(any(String.class))).thenReturn(msg);
        validator.validateFieldControlReturnPO(returnPurchaseOrders);
    }
    
    @Test
    public void validatePurchasingGroupEmptyTest() {
        Optional<PurchasingGroups> emptyOpt = Optional.empty();
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        when(result.first(PurchasingGroups.class)).thenReturn(emptyOpt);
        Mockito.when(messages.error(any(String.class),any(Object[].class))).thenReturn(msg);
        Mockito.when(msg.target(any(String.class))).thenReturn(msg);
        validator.validatePurchasingGroup("");
    }
    @Test
    public void validatePurchasingGroupNullTest() {
        Optional<PurchasingGroups> emptyOpt = Optional.empty();
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        when(result.first(PurchasingGroups.class)).thenReturn(emptyOpt);
        Mockito.when(messages.error(any(String.class),any(Object[].class))).thenReturn(msg);
        Mockito.when(msg.target(any(String.class))).thenReturn(msg);
        validator.validatePurchasingGroup(null);
    }
    @Test
    public void validatePurchasingGroupTest() {
        Optional<PurchasingGroups> emptyOpt = Optional.empty();
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        when(result.first(PurchasingGroups.class)).thenReturn(emptyOpt);
        Mockito.when(messages.error(any(String.class),any(Object[].class))).thenReturn(msg);
        Mockito.when(msg.target(any(String.class))).thenReturn(msg);
        validator.validatePurchasingGroup("test");
    }

    @Test
    public void validatePurchasingGroupIfExistNullTest() {
       Optional<PurchasingGroups> emptyOpt = Optional.of(purchasingGroups);
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        when(result.first(PurchasingGroups.class)).thenReturn(emptyOpt);
        List<PurchasingGroups> list=new ArrayList<>();
        list.add(purchasingGroups);
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        when(result.listOf(PurchasingGroups.class)).thenReturn(list);
        List<Row> rowValues = new ArrayList<>();
        Row row=Struct.create(Row.class);
        row.put("ID", "201");
        row.put("itemType_code", "202");
        Optional<Row> opt = Optional.of(row);
        rowValues.add(row);
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        when(result.list()).thenReturn(rowValues);
        when(result.first()).thenReturn(opt);
        when(result.iterator()).thenReturn(rowValues.iterator());
        validator.validatePurchasingGroupIfExist("test");
    }

    @Test
    public void validateItemNumerTest() {
        Optional<PurchasingGroups> emptyOpt = Optional.empty();
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        when(result.first(PurchasingGroups.class)).thenReturn(emptyOpt);
        Mockito.when(messages.error(any(String.class),any(Object[].class))).thenReturn(msg);
        Mockito.when(msg.target(any(String.class))).thenReturn(msg);
        validator.validateItemNumer("");
    }

    @Test
    public void validateReasonTest(){
        Optional<PurchasingGroups> emptyOpt = Optional.empty();
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        when(result.first(PurchasingGroups.class)).thenReturn(emptyOpt);
        Mockito.when(messages.error(any(String.class),any(Object[].class))).thenReturn(msg);
        Mockito.when(msg.target(any(String.class))).thenReturn(msg);
        validator.validateReason("");
    }

    @Test
    public void validateReasonIfExistElseTest(){
        Optional<PurchasingGroups> emptyOpt = Optional.of(purchasingGroups);
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        when(result.first(PurchasingGroups.class)).thenReturn(emptyOpt);
        List<PurchasingGroups> list=new ArrayList<>();
        list.add(purchasingGroups);
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        when(result.listOf(PurchasingGroups.class)).thenReturn(list);
        List<Row> rowValues = new ArrayList<>();
        Row row=Struct.create(Row.class);
        row.put("ID", "201");
        row.put("itemType_code", "202");
        Optional<Row> opt = Optional.of(row);
        rowValues.add(row);
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        when(result.list()).thenReturn(rowValues);
        when(result.first()).thenReturn(opt);
        when(result.iterator()).thenReturn(rowValues.iterator());
        validator.validateReasonIfExist("test");
    }

    @Test
    public void validatePersonResponsibleNullTest(){
          validator.validatePersonResponsible(null);
    }

    @Test
    public void validatePersonResponsibleElseTest(){
        BusinessPartners  businessPartners=Struct.create(BusinessPartners.class);
        businessPartners.setBusinessPartnerType("test");
        Optional<BusinessPartners> emptyOpt = Optional.of(businessPartners);
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        when(result.first(BusinessPartners.class)).thenReturn(emptyOpt);
        List<BusinessPartners> list=new ArrayList<>();
        list.add(businessPartners);
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        when(result.listOf(BusinessPartners.class)).thenReturn(list);
        List<Row> rowValues = new ArrayList<>();
        Row row=Struct.create(Row.class);
        row.put("ID", "201");
        row.put("itemType_code", "202");
        Optional<Row> opt = Optional.of(row);
        rowValues.add(row);
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        when(result.list()).thenReturn(rowValues);
        when(result.first()).thenReturn(opt);
        when(result.iterator()).thenReturn(rowValues.iterator());
        when(configurationDao.getSupplierData(any())).thenReturn(result);
        Mockito.when(messages.error(any(String.class),any(Object[].class))).thenReturn(msg);
        Mockito.when(msg.target(any(String.class))).thenReturn(msg);
        validator.validatePersonResponsible("test");
    }

    @Test
    public void testValidatePersonResponsibleForSameBusinessType(){
        BusinessPartners  businessPartners=Struct.create(BusinessPartners.class);
        businessPartners.setBusinessPartnerType("PERRES");
        Optional<BusinessPartners> emptyOpt = Optional.of(businessPartners);
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        when(result.first(BusinessPartners.class)).thenReturn(emptyOpt);
        List<BusinessPartners> list=new ArrayList<>();
        list.add(businessPartners);
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        when(result.listOf(BusinessPartners.class)).thenReturn(list);
        List<Row> rowValues = new ArrayList<>();
        Row row=Struct.create(Row.class);
        row.put("ID", "201");
        row.put("itemType_code", "202");
        Optional<Row> opt = Optional.of(row);
        rowValues.add(row);
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        when(result.list()).thenReturn(rowValues);
        when(result.first()).thenReturn(opt);
        when(result.iterator()).thenReturn(rowValues.iterator());
        when(configurationDao.getSupplierData(any())).thenReturn(result);
        Mockito.when(messages.error(any(String.class),any(Object[].class))).thenReturn(msg);
        Mockito.when(msg.target(any(String.class))).thenReturn(msg);
        validator.validatePersonResponsible("PERRES");
    }

    @Test
    public void validateIfReturnPurchaseOrderExistsForComplaintElseTest() {
        when(returnPurchaseOrderService.getReturnPurchaseOrderDetailsBasedOnComplaintId(any(String.class))).thenReturn(null);
         validator.validateIfReturnPurchaseOrderExistsForComplaint("8");
    }

    @Test
    public void validateIfReturnPurchaseOrderExistsNullTest() {
        ReturnPurchaseOrders returnPurchaseOrders=Struct.create(ReturnPurchaseOrders.class);
        when(returnPurchaseOrderService.getReturnPurchaseOrderDetails(any(String.class))).thenReturn(returnPurchaseOrders);
        validator.validateIfReturnPurchaseOrderExists("8");
    }

}