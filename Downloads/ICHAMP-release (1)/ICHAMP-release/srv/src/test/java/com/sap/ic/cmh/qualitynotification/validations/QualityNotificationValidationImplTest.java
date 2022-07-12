package com.sap.ic.cmh.qualitynotification.validations;

import cds.gen.masterdataservice.BusinessPartners;
import cds.gen.qualitynotificationservice.Defects;
import cds.gen.qualitynotificationservice.QualityNotifications;
import com.sap.cds.Result;
import com.sap.cds.Row;
import com.sap.cds.Struct;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.messages.Message;
import com.sap.cds.services.messages.Messages;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.ic.cmh.businessobjects.service.BusinessObjectService;
import com.sap.ic.cmh.configuration.validations.ConfigurationFieldsValidation;
import com.sap.ic.cmh.configuration.persistency.ConfigurationDao;
import com.sap.ic.cmh.configuration.validations.MasterDataValidation;
import com.sap.ic.cmh.gen.MessageKeys;
import com.sap.ic.cmh.qualitynotification.persistency.QualityNotificationDao;
import com.sap.ic.cmh.qualitynotification.service.QualityNotificationService;
import com.sap.ic.cmh.utils.SecurityValidator;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.*;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

public class QualityNotificationValidationImplTest {

    @InjectMocks
    @Autowired
    QualityNotificationValidationImpl qualityNotificationValidationImpl;
    @Mock
    protected PersistenceService mockDb;
    @Mock
    ConfigurationDao configurationDao;
    @Mock
    ConfigurationFieldsValidation configurationFieldsValidation;
    @Mock
    Messages messages;
    @Mock
    Message message;
    @Mock
    Result result;
    @Mock
    MasterDataValidation masterDataValidation;
    @Mock
    QualityNotificationService qualityNotificationService;
    @Mock
    BusinessObjectService businessObjectService;

    @Mock
    SecurityValidator securityValidator;

    @Mock
	QualityNotificationDao qualityNotificationDao;
    @Mock
    Result defectGroupCodeResult;

    private BusinessPartners  partners;
    private QualityNotifications qn;
    private QualityNotifications qualityNotifications;
    private List<QualityNotifications> boList = new ArrayList<>();
    private Row row;
    private Optional<Row> opt;
    private List<BusinessPartners> partnerList = new ArrayList<>();
    private Defects defect;

    @Before
    public void beforeClass(){
        MockitoAnnotations.openMocks(this);
        row = Struct.create(Row.class);
        qn = Struct.create(QualityNotifications.class);
        qn.setId("111");
        qn.setStatusCode("QNCRTD");
        qn.setComplaintId("444");
        qn.setMaterialId("MATT");
        qn.setCompanyId("COMP");
        qn.setPlantId("F002");
        qn.setSupplierId("SUPP");
        qn.setPurchasingOrganizationId("ORG");
        qn.setQnType("ADMIN");
        qn.setPersonResponsibleId("RESP_ID");
        qn.setSupplierRole("SUPP_ADMIN");
        qn.setPersonResponsibleRole("PRSN_RESP_ADMIN");
        qualityNotifications = Struct.create(QualityNotifications.class);
        qualityNotifications.setId(qn.getId());
        qualityNotifications.setStatusCode(qn.getStatusCode());
        qualityNotifications.setComplaintId(qn.getComplaintId());
        qualityNotifications.setCompanyId((qn.getCompanyId()));
        qualityNotifications.setPlantId(qn.getPlantId());
        qualityNotifications.setSupplierId(qn.getSupplierId());
        qualityNotifications.setPurchasingOrganizationId(qn.getPurchasingOrganizationId());
        qualityNotifications.setQnType(qn.getQnType());
        qualityNotifications.setPersonResponsibleId(qn.getPersonResponsibleId());
        qualityNotifications.setSupplierRole(qn.getSupplierRole());
        qualityNotifications.setPersonResponsibleRole(qn.getPersonResponsibleRole());
        defect = Struct.create(Defects.class);
        defect.setDescription("desc");
        defect.setDefectCodeCode("1");
        defect.setDefectGroupCode("QM-E");
        Map<String,Object> map = new HashMap<>();
        map.put("defect", defect);
        qualityNotifications.setDefect(map);
        boList.add(qn);

        partners = Struct.create(BusinessPartners.class);
        partners.setId("RESP_ID");      
        partners.setBusinessPartnerNumber("1234");
        partners.setIsMarkedForDeletion(true);
        partners.setBusinessPartnerType("SUP");
        partnerList.add(partners);

    }

    @Test
    public void testValidateMandatoryFieldsElse() {
        partners.setIsMarkedForDeletion(false);
        partnerList.add(partners);
        List<Row> rowvalues = new ArrayList<>();
        row.put("navigationDestination", "navigationdestination");
        row.put("1", "1");
        row.put("RESP_ID", "RESP_ID");
        row.put("QM-E", "QM-E");
        opt = Optional.of(row);
        rowvalues.add(row);
        Mockito.when(result.list()).thenReturn(rowvalues);
        Mockito.when(result.first()).thenReturn(opt);
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.listOf(BusinessPartners.class)).thenReturn(partnerList);
        when(configurationDao.getSupplierData("RESP_ID")).thenReturn(result);
        when(qualityNotificationDao.getDefectCode("1")).thenReturn(result);
        when(qualityNotificationDao.getDefectGroup("QM-E")).thenReturn(result);
        when(messages.error(MessageKeys.DEFECT_GROUP_MANDATORY)).thenReturn(message);
        when(messages.error(MessageKeys.PERS_RESP_IS_MANDATORY)).thenReturn(message);
        when(messages.error(MessageKeys.WRONG_PERSON_RESP_SELECTED)).thenReturn(message);
        qualityNotificationValidationImpl.validateMandatoryFields(qualityNotifications);
    }

    @Test
    public void testValidateMandatoryFieldsForPartnerTypePERRES() {
        partners.setIsMarkedForDeletion(false);
        partners.setBusinessPartnerType("PERRES");
        partnerList.add(partners);
        List<Row> rowvalues = new ArrayList<>();
        row.put("navigationDestination", "navigationdestination");
        row.put("1", "1");
        row.put("RESP_ID", "RESP_ID");
        row.put("QM-E", "QM-E");
        opt = Optional.of(row);
        rowvalues.add(row);
        Mockito.when(result.list()).thenReturn(rowvalues);
        Mockito.when(result.first()).thenReturn(opt);
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.listOf(BusinessPartners.class)).thenReturn(partnerList);
        when(configurationDao.getSupplierData("RESP_ID")).thenReturn(result);
        when(qualityNotificationDao.getDefectCode("1")).thenReturn(result);
        when(qualityNotificationDao.getDefectGroup("QM-E")).thenReturn(result);
        when(messages.error(MessageKeys.DEFECT_GROUP_MANDATORY)).thenReturn(message);
        when(messages.error(MessageKeys.PERS_RESP_IS_MANDATORY)).thenReturn(message);
        when(messages.error(MessageKeys.WRONG_PERSON_RESP_SELECTED)).thenReturn(message);
        qualityNotificationValidationImpl.validateMandatoryFields(qualityNotifications);
    }

    @Test
    public void testValidateMandatoryFieldsDefectNull() {
        qualityNotifications.setDefect(null);
        List<Row> rowvalues = new ArrayList<>();
        row.put("navigationDestination", "navigationdestination");
        row.put("1", "1");
        row.put("RESP_ID", "RESP_ID");
        row.put("QM-E", "QM-E");
        opt = Optional.of(row);
        rowvalues.add(row);
        Mockito.when(result.list()).thenReturn(rowvalues);
        Mockito.when(result.first()).thenReturn(opt);
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.listOf(BusinessPartners.class)).thenReturn(partnerList);
        when(configurationDao.getSupplierData("RESP_ID")).thenReturn(result);
        when(messages.error(MessageKeys.DEFECTS_MANDATORY)).thenReturn(message);
        when(messages.error(MessageKeys.PERSON_RESP_MARKED_FOR_DELETION)).thenReturn(message);
        qualityNotificationValidationImpl.validateMandatoryFields(qualityNotifications);
    }
    @Test
    public void testValidateMandatoryFieldsDefectGroupCode() {
        qualityNotifications.getDefect().setDefectCodeCode(null);
        qualityNotifications.getDefect().setDefectGroupCode("1");
        List<Row> rowvalues = new ArrayList<>();
        row.put("navigationDestination", "navigationdestination");
        row.put("1", "1");
        row.put("RESP_ID", "RESP_ID");
        row.put("QM-E", "QM-E");
        opt = Optional.of(row);
        rowvalues.add(row);
        Mockito.when(result.list()).thenReturn(rowvalues);
        Mockito.when(result.first()).thenReturn(opt);
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.listOf(BusinessPartners.class)).thenReturn(partnerList);
        when(configurationDao.getSupplierData("RESP_ID")).thenReturn(result);
        when(messages.error(MessageKeys.DEFECTS_MANDATORY)).thenReturn(message);
        when(messages.error(MessageKeys.PERSON_RESP_MARKED_FOR_DELETION)).thenReturn(message);
        when(messages.error(MessageKeys.DEFECT_CODE_MANDATORY)).thenReturn(message);
        qualityNotificationValidationImpl.validateMandatoryFields(qualityNotifications);
    }

    @Test
    public void testValidateMandatoryFieldsDefectCodeCode() {
        qualityNotifications.getDefect().setDefectCodeCode("1");
        qualityNotifications.getDefect().setDefectGroupCode(null);
        List<Row> rowvalues = new ArrayList<>();
        row.put("navigationDestination", "navigationdestination");
        row.put("1", "1");
        row.put("RESP_ID", "RESP_ID");
        row.put("QM-E", "QM-E");
        opt = Optional.of(row);
        rowvalues.add(row);
        Mockito.when(result.list()).thenReturn(rowvalues);
        Mockito.when(result.first()).thenReturn(opt);
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.listOf(BusinessPartners.class)).thenReturn(partnerList);
        when(configurationDao.getSupplierData("RESP_ID")).thenReturn(result);
        when(messages.error(MessageKeys.DEFECTS_MANDATORY)).thenReturn(message);
        when(messages.error(MessageKeys.PERSON_RESP_MARKED_FOR_DELETION)).thenReturn(message);
        when(messages.error(MessageKeys.DEFECT_GROUP_MANDATORY)).thenReturn(message);
        qualityNotificationValidationImpl.validateMandatoryFields(qualityNotifications);
    }

    // @Test
    // public void testValidateMandatoryFieldsDefectCode() {
    //     qualityNotifications.getDefect().setDefectCodeCode("34");
    //     qualityNotifications.getDefect().setDefectGroupCode("23");
    //     List<Row> rowvalues = new ArrayList<>();
    //     row.put("navigationDestination", "navigationdestination");
    //     row.put("1", "1");
    //     row.put("RESP_ID", "RESP_ID");
    //     row.put("QM-E", "QM-E");
    //     opt = Optional.of(row);
    //     rowvalues.add(row);
    //     Mockito.when(result.list()).thenReturn(rowvalues);
    //     Mockito.when(result.first()).thenReturn(opt);
    //     when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
    //     Result result1 = mock(Result.class);
    //     when(qualityNotificationDao.getDefectCode(any())).thenReturn(result1);
    //     when(qualityNotificationDao.getDefectGroup(any())).thenReturn(result1);
    //     Optional<Row> row1 = mock(Optional.class);
    //     when(result1.first()).thenReturn(row1);
    //     when(row1.isPresent()).thenReturn(false);
    //     when(result.listOf(BusinessPartners.class)).thenReturn(partnerList);
    //     when(configurationDao.getSupplierData("RESP_ID")).thenReturn(result);
    //     when(messages.error(MessageKeys.DEFECT_GROUP_NOT_EXIST)).thenReturn(message);
    //     when(messages.error(MessageKeys.PERSON_RESP_MARKED_FOR_DELETION)).thenReturn(message);
    //     when(messages.error(MessageKeys.DEFECT_CODE_NOT_EXIST)).thenReturn(message);
    //     qualityNotificationValidationImpl.validateMandatoryFields(qualityNotifications);
    // }

    @Test
    public void testValidateMandatoryFields() {
        List<Row> rowvalues = new ArrayList<>();
        row.put("navigationDestination", "navigationdestination");
        row.put("1", "1");
        row.put("RESP_ID", "RESP_ID");
        row.put("QM-E", "QM-E");
        opt = Optional.of(row);
        rowvalues.add(row);
        Mockito.when(result.list()).thenReturn(rowvalues);
        Mockito.when(result.first()).thenReturn(opt);
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.listOf(BusinessPartners.class)).thenReturn(partnerList);
        when(configurationDao.getSupplierData("RESP_ID")).thenReturn(result);
       // when(messages.error(MessageKeys.PERS_RESP_IS_NOT_VALID)).thenReturn(message);
        when(qualityNotificationDao.getDefectCode("1")).thenReturn(result);
        when(qualityNotificationDao.getDefectGroup("QM-E")).thenReturn(result);
        when(messages.error(MessageKeys.DEFECT_GROUP_MANDATORY)).thenReturn(message);
        when(messages.error(MessageKeys.PERS_RESP_IS_MANDATORY)).thenReturn(message);
        when(messages.error(MessageKeys.PERSON_RESP_MARKED_FOR_DELETION)).thenReturn(message);
        qualityNotificationValidationImpl.validateMandatoryFields(qualityNotifications);
    }

   @Test
    public void testValidateMandatoryFieldsNotificationType(){
        qualityNotifications.setQnType("");
        when(messages.error(MessageKeys.NOTIFICATION_TYPE_NOT_CONFIGURED)).thenReturn(message);
        when(configurationDao.getSupplierData("RESP_ID")).thenReturn(result);
       when(messages.error(MessageKeys.PERS_RESP_IS_NOT_VALID)).thenReturn(message);
        when(messages.error(MessageKeys.DEFECT_GROUP_MANDATORY)).thenReturn(message);
        qualityNotificationValidationImpl.validateMandatoryFields(qualityNotifications);
    }
    @Test
    public void testValidateMandatoryFieldsSupplierRole(){
        qualityNotifications.setSupplierRole("");
        when(messages.error(MessageKeys.SUPPLIER_ROLE_NOT_CONFIGURED)).thenReturn(message);
        when(configurationDao.getSupplierData("RESP_ID")).thenReturn(result);
        when(messages.error(MessageKeys.PERS_RESP_IS_NOT_VALID)).thenReturn(message);
         when(messages.error(MessageKeys.DEFECT_GROUP_MANDATORY)).thenReturn(message);
        qualityNotificationValidationImpl.validateMandatoryFields(qualityNotifications);
    }
    @Test
    public void testValidateMandatoryFieldsPersonResponsibleRole(){
        qualityNotifications.setPersonResponsibleRole("");
        when(messages.error(MessageKeys.PERS_RESP_ROLE_NOT_CONFIGURED)).thenReturn(message);
        when(configurationDao.getSupplierData("RESP_ID")).thenReturn(result);
        when(messages.error(MessageKeys.PERS_RESP_IS_NOT_VALID)).thenReturn(message);
         when(messages.error(MessageKeys.DEFECT_GROUP_MANDATORY)).thenReturn(message);
        qualityNotificationValidationImpl.validateMandatoryFields(qualityNotifications);
    }

    @Test
    public void testValidateMandatoryFieldsPersonResponsible(){
        Map<String, Object> map = new HashMap<>();
        map.put("person_responsible",new Object());
        row.put("defectCode_code", "1");
        row.put("RESP_ID", "RESP_ID");
        row.put("defectGroup_code", "QM-E");
        qualityNotifications.setPersonResponsible(map);
        when(messages.error(MessageKeys.PERS_RESP_IS_MANDATORY)).thenReturn(message);
        when(configurationDao.getSupplierData("RESP_ID")).thenReturn(result);
        when(messages.error(MessageKeys.PERS_RESP_IS_NOT_VALID)).thenReturn(message);
        when(messages.error(MessageKeys.DEFECT_GROUP_MANDATORY)).thenReturn(message);
        qualityNotificationValidationImpl.validateMandatoryFields(qualityNotifications);
    }

    @Test
    public void testValidateMandatoryFieldsPersonResponsibleNull(){
        row.put("defectCode_code", "1");
        row.put("RESP_ID", "RESP_ID");
        row.put("defectGroup_code", "QM-E");
        qualityNotifications.setPersonResponsibleId(null);
        when(messages.error(MessageKeys.PERS_RESP_IS_MANDATORY)).thenReturn(message);
        when(configurationDao.getSupplierData("RESP_ID")).thenReturn(result);
        when(messages.error(MessageKeys.PERS_RESP_IS_NOT_VALID)).thenReturn(message);
        when(messages.error(MessageKeys.DEFECT_GROUP_MANDATORY)).thenReturn(message);
        qualityNotificationValidationImpl.validateMandatoryFields(qualityNotifications);
    }

//    @Test
//    public void testValidateMandatoryFieldsDefectsNull() {
//        qualityNotifications.setDefect(null);
//        when(configurationDao.getSupplierData("RESP_ID")).thenReturn(result);
//        List<Row> rowvalues = new ArrayList<>();
//        row.put("destination", "ComplaintID");
//        opt = Optional.of(row);
//        rowvalues.add(row);
//        Mockito.when(result.list()).thenReturn(rowvalues);
//        Mockito.when(result.first()).thenReturn(opt);
//        Mockito.when(result.listOf(BusinessPartners.class)).thenReturn(partnerList);
//        when(messages.error(MessageKeys.DEFECTS_MANDATORY)).thenReturn(message);
//        qualityNotificationValidationImpl.validateMandatoryFields(qualityNotifications);
//    }
//    @Test
//    public void testValidateMandatoryFieldsDefectsCodeNull() {
//        defect.setDefectCodeCode(null);
//        qualityNotifications.setDefect(defect);
//        when(configurationDao.getSupplierData("RESP_ID")).thenReturn(result);
//        List<Row> rowvalues = new ArrayList<>();
//        row.put("destination", "ComplaintID");
//        opt = Optional.of(row);
//        rowvalues.add(row);
//        Mockito.when(result.list()).thenReturn(rowvalues);
//        Mockito.when(result.first()).thenReturn(opt);
//        Mockito.when(result.listOf(BusinessPartners.class)).thenReturn(partnerList);
//        when(messages.error(MessageKeys.BUSINESS_PARTNER_MARKED_FOR_DELETION)).thenReturn(message);
//        qualityNotificationValidationImpl.validateMandatoryFields(qualityNotifications);
//    }


//    @Test
//    public void testValidateMandatoryFieldsDefectsGroupCodeNull() {
//        defect.setDefectGroupCode(null);
//        qualityNotifications.setDefect(defect);
//        when(configurationDao.getSupplierData("RESP_ID")).thenReturn(result);
//        List<Row> rowvalues = new ArrayList<>();
//        row.put("destination", "ComplaintID");
//        opt = Optional.of(row);
//        rowvalues.add(row);
//        Mockito.when(result.list()).thenReturn(rowvalues);
//        Mockito.when(result.first()).thenReturn(opt);
//        Mockito.when(result.listOf(BusinessPartners.class)).thenReturn(partnerList);
//        when(messages.error(MessageKeys.DEFECT_GROUP_MANDATORY)).thenReturn(message);
//        qualityNotificationValidationImpl.validateMandatoryFields(qualityNotifications);
//    }

//    @Test
//    public void testValidateMandatoryFieldsDefects() {
//        defect.setDefectGroupCode("group");
//        defect.setDefectCodeCode("code");
//        qualityNotifications.setDefect(defect);
//        when(configurationDao.getSupplierData("RESP_ID")).thenReturn(result);
//        List<Row> rowvalues = new ArrayList<>();
//        row.put("destination", "ComplaintID");
//        opt = Optional.of(row);
//        rowvalues.add(row);
//        Mockito.when(qualityNotificationDao.getDefectCode(defect.getDefectCodeCode())).thenReturn(result);
//        Mockito.when(qualityNotificationDao.getDefectGroup(defect.getDefectGroupCode())).thenReturn(result);
//        Mockito.when(result.list()).thenReturn(rowvalues);
//        Mockito.when(result.first()).thenReturn(opt);
//        Mockito.when(result.listOf(BusinessPartners.class)).thenReturn(partnerList);
//        qualityNotificationValidationImpl.validateMandatoryFields(qualityNotifications);
//    }

    @Test
    public void testValidateFieldControlQN(){
        qn.setStatusCode("NEW");
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.listOf(QualityNotifications.class)).thenReturn(boList);
        when(qualityNotificationService.getQualityNotificationDetails(qn.getId())).thenReturn(qualityNotifications);
        qualityNotificationValidationImpl.validateFieldControlQN(qn);
    }

    @Test
    public void testValidateFieldControlQnNull(){
        qn.setStatusCode("NEW");
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.listOf(QualityNotifications.class)).thenReturn(boList);
        when(qualityNotificationService.getQualityNotificationDetails(qn.getId())).thenReturn(null);
        qualityNotificationValidationImpl.validateFieldControlQN(qn);
    }

    @Test
    public void testValidateFieldControlQnTypeNull(){
        qn.setStatusCode("NEW");
        qn.setQnType(null);
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.listOf(QualityNotifications.class)).thenReturn(boList);
        when(qualityNotificationService.getQualityNotificationDetails(qn.getId())).thenReturn(qualityNotifications);
        qualityNotificationValidationImpl.validateFieldControlQN(qn);
    }

    @Test
    public void testValidateFieldControlQnPersonResponsibleIdNull(){
        qn.setStatusCode("NEW");
        qn.setPersonResponsibleId(null);
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.listOf(QualityNotifications.class)).thenReturn(boList);
        when(qualityNotificationService.getQualityNotificationDetails(qn.getId())).thenReturn(qualityNotifications);
        qualityNotificationValidationImpl.validateFieldControlQN(qn);
    }

    @Test
    public void testValidateFieldControlQnPurchaseOrderNumberNull(){
        qn.setStatusCode("NEW");
        qn.setPurchaseOrderNumber(null);
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.listOf(QualityNotifications.class)).thenReturn(boList);
        when(qualityNotificationService.getQualityNotificationDetails(qn.getId())).thenReturn(qualityNotifications);
        qualityNotificationValidationImpl.validateFieldControlQN(qn);
    }

    @Test
    public void testValidateFieldControlQnPurchaseOrderItemNull(){
        qn.setStatusCode("NEW");
        qn.setPurchaseOrderItem(null);
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.listOf(QualityNotifications.class)).thenReturn(boList);
        when(qualityNotificationService.getQualityNotificationDetails(qn.getId())).thenReturn(qualityNotifications);
        qualityNotificationValidationImpl.validateFieldControlQN(qn);
    }

    @Test
    public void testValidateFieldControlQnDefectNull(){
        qn.setStatusCode("NEW");
        qn.setDefect(null);
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.listOf(QualityNotifications.class)).thenReturn(boList);
        when(qualityNotificationService.getQualityNotificationDetails(qn.getId())).thenReturn(qualityNotifications);
        qualityNotificationValidationImpl.validateFieldControlQN(qn);
    }

    @Test
    public void testValidateFieldControlQNStatus(){
        qn.setStatusCode("QNCMPL");
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.listOf(QualityNotifications.class)).thenReturn(boList);
        when(qualityNotificationService.getQualityNotificationDetails(qn.getId())).thenReturn(qualityNotifications);
        when(messages.error(MessageKeys.ATTRIBUTES_NOT_EDITABLE)).thenReturn(message);
        qualityNotificationValidationImpl.validateFieldControlQN(qn);
    }

    @Test
    public void testValidateFieldControlQNType(){
        qn.setStatusCode("QNINPR");
        qn.setQnType("GRF");
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.listOf(QualityNotifications.class)).thenReturn(boList);
        when(qualityNotificationService.getQualityNotificationDetails(qn.getId())).thenReturn(qualityNotifications);
        when(messages.error(MessageKeys.NOTIFICATION_TYPE_NOT_EDITABLE)).thenReturn(message);
        qualityNotificationValidationImpl.validateFieldControlQN(qn);
    }

    @Test
    public void testValidateFieldControlPersonResponsibleId(){
        qn.setStatusCode("QNINPR");
        qn.setPersonResponsibleId("GRF");
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.listOf(QualityNotifications.class)).thenReturn(boList);
        when(qualityNotificationService.getQualityNotificationDetails(qn.getId())).thenReturn(qualityNotifications);
        when(messages.error(MessageKeys.PERSON_RESPONISBLE_NOT_EDITABLE)).thenReturn(message);
        qualityNotificationValidationImpl.validateFieldControlQN(qn);
    }

    @Test
    public void testValidateFieldControlPurchaseOrderNumber(){
        qn.setStatusCode("QNINPR");
        qn.setPurchaseOrderNumber("DF");
        qualityNotifications.setPurchaseOrderNumber("Snb");

        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.listOf(QualityNotifications.class)).thenReturn(boList);
        when(qualityNotificationService.getQualityNotificationDetails(qn.getId())).thenReturn(qualityNotifications);
        when(messages.error(MessageKeys.PURCHASE_ORDER_NOT_EDITABLE)).thenReturn(message);
        qualityNotificationValidationImpl.validateFieldControlQN(qn);
    }

    @Test
    public void testValidateFieldControlPurchaseOrderItem(){
        qn.setStatusCode("QNINPR");
        qn.setPurchaseOrderItem("CAR");
        qualityNotifications.setPurchaseOrderItem("CARQE");
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.listOf(QualityNotifications.class)).thenReturn(boList);
        when(qualityNotificationService.getQualityNotificationDetails(qn.getId())).thenReturn(qualityNotifications);
        when(messages.error(MessageKeys.PURCHASE_ORDER_ITEM_NOT_EDITABLE)).thenReturn(message);
        qualityNotificationValidationImpl.validateFieldControlQN(qn);
    }

    @Test
    public void testValidateFieldControlDefect(){
        qn.setStatusCode("QNINPR");
        Defects defect = Struct.create(Defects.class);
        defect.setDefectCodeCode("DFTH");
        qn.setDefect(defect);
        Defects defect1 = Struct.create(Defects.class);
        defect1.setDefectCodeCode("GBHJ");
        qualityNotifications.setDefect(defect1);
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.listOf(QualityNotifications.class)).thenReturn(boList);
        when(qualityNotificationService.getQualityNotificationDetails(qn.getId())).thenReturn(qualityNotifications);
        when(messages.error(MessageKeys.DEFECT_CODE_NOT_EDITABLE)).thenReturn(message);
        qualityNotificationValidationImpl.validateFieldControlQN(qn);
    }

    @Test
    public void testValidateFieldControlDefectGroupCode(){
        qn.setStatusCode("QNINPR");
        Defects defect = Struct.create(Defects.class);
        defect.setDefectCodeCode("cVMMBNN");
        defect.setDefectGroupCode("SDFGH");
        qn.setDefect(defect);
        Defects defect1 = Struct.create(Defects.class);
        defect1.setDefectCodeCode("cVMMBNN");
        defect1.setDefectGroupCode("FASD");
        qualityNotifications.setDefect(defect1);
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.listOf(QualityNotifications.class)).thenReturn(boList);
        when(qualityNotificationService.getQualityNotificationDetails(qn.getId())).thenReturn(qualityNotifications);
        when(messages.error(MessageKeys.DEFECT_GROUP_NOT_EDITABLE)).thenReturn(message);
        qualityNotificationValidationImpl.validateFieldControlQN(qn);
    }

    @Test
    public void testValidateFieldControlDefectDescription(){
        qn.setStatusCode("QNINPR");
        Defects defect = Struct.create(Defects.class);
        defect.setDefectCodeCode("cVMMBNN");
        defect.setDefectGroupCode("SDFGH");
        defect.setDescription("welcome");
        qn.setDefect(defect);
        Defects defect1 = Struct.create(Defects.class);
        defect1.setDefectCodeCode("cVMMBNN");
        defect1.setDefectGroupCode("SDFGH");
        defect1.setDescription("welcome1");
        qualityNotifications.setDefect(defect1);
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.listOf(QualityNotifications.class)).thenReturn(boList);
        when(qualityNotificationService.getQualityNotificationDetails(qn.getId())).thenReturn(qualityNotifications);
        when(messages.error(MessageKeys.DEFECT_DESCRIPTION_NOT_EDITABLE)).thenReturn(message);
        qualityNotificationValidationImpl.validateFieldControlQN(qn);
    }

    @Test
    public void testValidateIfQNExistsForComplaint() {
        when(qualityNotificationService.getQualityNotificationDetailsByComplaintId("1234")).thenReturn(qualityNotifications);
        when(messages.error(MessageKeys.QUALITY_NOTIFICATION_EXISTS)).thenReturn(message);
        qualityNotificationValidationImpl.validateIfQNExistsForComplaint("1234");
    }
    @Test
    public void testValidateIfQNExistsForComplaintQnNull() {
        when(qualityNotificationService.getQualityNotificationDetailsByComplaintId("1234")).thenReturn(null);
        when(messages.error(MessageKeys.QUALITY_NOTIFICATION_EXISTS)).thenReturn(message);
        qualityNotificationValidationImpl.validateIfQNExistsForComplaint("1234");
    }

    @Test
    public void testValidateifBOIsRelevant() {
        when(businessObjectService.checkIfBOIsRelevant("1234", "F0667")).thenReturn(true);
        when(messages.error(MessageKeys.QUALITY_NOTIFICATION_ALWAYS_RELEVANT)).thenReturn(message);
        qualityNotificationValidationImpl.validateIfBOIsRelevant("1234", "F0667");

    }

    @Test
    public void testValidateifBOIsRelevantFalse() {
        when(businessObjectService.checkIfBOIsRelevant("1234", "F0667")).thenReturn(false);
        when(messages.error(MessageKeys.QUALITY_NOTIFICATION_ALWAYS_RELEVANT)).thenReturn(message);
        qualityNotificationValidationImpl.validateIfBOIsRelevant("1234", "F");
    }

    @Test
    public void testValidateIfQNExists() {
        when(qualityNotificationService.getQualityNotificationDetails("23")).thenReturn(qualityNotifications);
        qualityNotificationValidationImpl.validateIfQNExists("23");
    }

    @Test
    public void testValidateIfQNExistsQnNull() {
        when(qualityNotificationService.getQualityNotificationDetails("23")).thenReturn(null);
        when(messages.error(MessageKeys.QUALITY_NOTIFICATION_NOT_EXIST)).thenReturn(message);
        qualityNotificationValidationImpl.validateIfQNExists("23");
    }
    @Test
    public void testValidateQNFreeTextFields() {
        qualityNotifications.getDefect().setDescription("AnyDesc");
        when(messages.error(MessageKeys.INVALID_DEFECT_DESCRIPTION)).thenReturn(message);
        qualityNotificationValidationImpl.validateQNFreeTextFields(qualityNotifications);
    }

    @Test
    public void testValidateQNFreeTextFields1() {
        when(securityValidator.isValidText(any())).thenReturn(true);
        when(messages.error(MessageKeys.INVALID_DEFECT_DESCRIPTION)).thenReturn(message);
        qualityNotificationValidationImpl.validateQNFreeTextFields(qualityNotifications);
    }

    @Test
    public void testValidatePurchaseOrderNumberNegative() {
        qn.setPurchaseOrderNumber("-123");
        when(messages.error(MessageKeys.PURCHASE_NUMBER_NOT_VALID)).thenReturn(message);
        qualityNotificationValidationImpl.validatePurchaseOrderNumber(qn.getPurchaseOrderNumber());
    }

    @Test
    public void testValidatePurchaseOrderItemNumberNegative() {
        qn.setPurchaseOrderItem("-123");
        when(messages.error(MessageKeys.PURCHASE_ITEM_NOT_VALID)).thenReturn(message);
        qualityNotificationValidationImpl.validatePurchaseOrderItemNumber(qn.getPurchaseOrderItem());
    }

}