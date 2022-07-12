package com.sap.ic.cmh.supplierissueprocess.validations;

import cds.gen.masterdataservice.BusinessPartners;
import cds.gen.supplierissueprocessservice.Supplier8DProcesses;
import cds.gen.qualitynotificationservice.QualityNotifications;
import com.sap.cds.Result;
import com.sap.cds.Row;
import com.sap.cds.Struct;
import com.sap.cds.services.messages.Message;
import com.sap.cds.services.messages.Messages;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.ic.cmh.businessobjects.service.BusinessObjectService;
import com.sap.ic.cmh.configuration.persistency.ConfigurationDao;
import com.sap.ic.cmh.configuration.validations.MasterDataValidation;
import com.sap.ic.cmh.gen.MessageKeys;
import com.sap.ic.cmh.supplierissueprocess.persistency.EightDDao;
import com.sap.ic.cmh.supplierissueprocess.service.EightDService;
import com.sap.ic.cmh.utils.SecurityValidator;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

public class EightDValidationImplTest {

    @InjectMocks
    @Autowired
    EightDValidationImpl validator;

    @Mock
    Messages messages;

    @Mock
    Message message;

    @Mock
    SecurityValidator securityValidator;

    @Mock
    EightDDao eightDDao;

    @Mock
    MasterDataValidation masterDataValidation;

    @Mock
    PersistenceService db;

    @Mock
    EightDService eightDService;

    @Mock
    BusinessObjectService businessObjectService;

    @Mock
    Result result;

    @Mock
    ConfigurationDao configurationDao;

    private Supplier8DProcesses eightD;
    private Supplier8DProcesses eightD1;

    private Row row;
    private Optional<Row> opt;
    private List<BusinessPartners> partnerList = new ArrayList<>();
    private BusinessPartners partners;

    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
        eightD = Struct.create(Supplier8DProcesses.class);
        eightD.setId("01");
        eightD.setComplaintId("Id");
        eightD.setSupplierIssueProcessesType("tyoe");
        eightD.setMaterialId("133424");
        eightD.setPlantId("F56");
        eightD.setSupplierId("D566");
        eightD.setPurchasingOrganizationId("567");
        eightD.setQuantity(BigDecimal.TEN);
        eightD.setPersonResponsibleId("abc");


        eightD1 = Struct.create(Supplier8DProcesses.class);
        eightD1.setId("01");
        eightD1.setComplaintId("Id");
        eightD1.setSupplierIssueProcessesType("TYOEA");
        eightD1.setMaterialId("133424");
        eightD1.setPlantId("F56");
        eightD1.setSupplierId("D566");
        eightD1.setPurchasingOrganizationId("567");
        eightD1.setQuantity(BigDecimal.TEN);
        eightD1.setPersonResponsibleId("ABCD");

        partners = Struct.create(BusinessPartners.class);
        partners.setId("ABCD");
        partners.setBusinessPartnerNumber("1234");
        partners.setIsMarkedForDeletion(true);
        partners.setBusinessPartnerType("SUP");
        partnerList.add(partners);

        row = Struct.create(Row.class);
    }

    @Test
    public void testValidateEightDFields() {
        when(messages.error(MessageKeys.INVALID_8D_SUPPLIER_PROCESS_TYPE)).thenReturn(message);
        when(configurationDao.getSupplierData("abc")).thenReturn(result);
        List<Row> rowvalues = new ArrayList<>();
        row.put("complaint", "ComplaintID");
        opt = Optional.of(row);
        rowvalues.add(row);
        when(result.listOf(BusinessPartners.class)).thenReturn(partnerList);
        when(result.first()).thenReturn(opt);
        when(messages.error(MessageKeys.PERSON_RESP_MARKED_FOR_DELETION)).thenReturn(message);
        validator.validateEightDFields(eightD);
    }

    @Test
    public void testValidateEightDFieldsSupplierIssueProcessesType() {
        eightD.setSupplierIssueProcessesType("FGRFVF");
        eightD.setPersonResponsibleId(null);
        when(securityValidator.isValidText(any())).thenReturn(true);
        validator.validateEightDFields(eightD);
    }

    @Test
    public void testValidateEightDFieldsNull() {
        when(messages.error(MessageKeys.INVALID_8D_SUPPLIER_PROCESS_TYPE)).thenReturn(message);
        when(configurationDao.getSupplierData("abc")).thenReturn(result);
        when(messages.error(MessageKeys.PERS_RESP_IS_NOT_VALID)).thenReturn(message);
        validator.validateEightDFields(eightD);
    }

    @Test
    public void testValidateEightDFieldsFalse() {
        partners.setIsMarkedForDeletion(false);
        partners.setBusinessPartnerType("PERRES");
        when(messages.error(MessageKeys.INVALID_8D_SUPPLIER_PROCESS_TYPE)).thenReturn(message);
        when(configurationDao.getSupplierData("abc")).thenReturn(result);
        List<Row> rowvalues = new ArrayList<>();
        row.put("complaint", "ComplaintID");
        opt = Optional.of(row);
        rowvalues.add(row);
        when(result.listOf(BusinessPartners.class)).thenReturn(partnerList);
        when(result.first()).thenReturn(opt);
        when(messages.error(MessageKeys.WRONG_PERSON_RESP_SELECTED)).thenReturn(message);
        validator.validateEightDFields(eightD);
    }

    @Test
    public void testValidateEightDFieldsPartnerTypeDiffValue() {
        partners.setIsMarkedForDeletion(false);
        partners.setBusinessPartnerType("PERS");
        when(messages.error(MessageKeys.INVALID_8D_SUPPLIER_PROCESS_TYPE)).thenReturn(message);
        when(configurationDao.getSupplierData("abc")).thenReturn(result);
        List<Row> rowvalues = new ArrayList<>();
        row.put("complaint", "ComplaintID");
        opt = Optional.of(row);
        rowvalues.add(row);
        when(result.listOf(BusinessPartners.class)).thenReturn(partnerList);
        when(result.first()).thenReturn(opt);
        when(messages.error(MessageKeys.WRONG_PERSON_RESP_SELECTED)).thenReturn(message);
        validator.validateEightDFields(eightD);
    }

    @Test
    public void testValidateFieldControlEightD() {
        List<Row> rowvalues = new ArrayList<>();
        row.put("ID", "ComplaintID");
        opt = Optional.of(row);
        rowvalues.add(row);
        when(eightDDao.getEightDBasedOnId(eightD.getId())).thenReturn(result);
        eightD.setStatusCode("S8DCMPL");
        Mockito.when(result.list()).thenReturn(rowvalues);
        Mockito.when(result.first()).thenReturn(opt);
        when(messages.error(MessageKeys.ATTRIBUTES_NOT_EDITABLE)).thenReturn(message);
        validator.validateFieldControlEightD(eightD);
    }

    @Test
    public void testValidateFieldControlEightDNotNew() {
        eightD.setStatusCode("S8DINPR");
        when(eightDService.getEightDDetails(eightD.getId())).thenReturn(eightD1);
        when(messages.error(MessageKeys.NOTIFICATION_TYPE_NOT_EDITABLE)).thenReturn(message);
        validator.validateFieldControlEightD(eightD);
    }

    @Test
    public void testValidateFieldControlEightDCLMSENT() {
        List<Row> rowvalues = new ArrayList<>();
        row.put("ID", "ComplaintID");
        opt = Optional.of(row);
        rowvalues.add(row);
        when(eightDDao.getEightDBasedOnId(eightD.getId())).thenReturn(result);
        eightD.setStatusCode("NEW");
        Mockito.when(result.list()).thenReturn(rowvalues);
        Mockito.when(result.first()).thenReturn(opt);
        when(messages.error(MessageKeys.ATTRIBUTES_NOT_EDITABLE)).thenReturn(message);
        validator.validateFieldControlEightD(eightD);
    }

    @Test
    public void testValidateIf8DExists() {
        when(eightDService
                .getEightDDetailsBasedOnComplaintId("1234")).thenReturn(eightD);
        when(messages.error(MessageKeys.SUPPLIER_EIGHTD_NOT_EXIST)).thenReturn(message);
        validator.validateIf8DExists("1234");
    }

    @Test
    public void testValidateIf8DExistsNotNull() {
        when(eightDService.getEightDDetails(any())).thenReturn(eightD);
        validator.validateIf8DExists("1234");
    }

    @Test
    public void testValidateIfBOIsRelevant() {
        when(messages.error(MessageKeys.SUPPLIER_EIGHTD_NOT_RELEVANT)).thenReturn(message);
        validator.validateifBOIsRelevant("1234", "234");
    }

    @Test
    public void testValidateIfBOIsRelevantNull() {
        when(messages.error(MessageKeys.SUPPLIER_EIGHTD_NOT_RELEVANT)).thenReturn(message);
        when(businessObjectService.checkIfBOIsRelevant(any(), any())).thenReturn(true);
        validator.validateifBOIsRelevant("1234", "234");
    }

    @Test
    public void testValidateSupplier8DProcessType() {
        when(messages.error(MessageKeys.SUPPLIER_TYPE_NOT_CONFIGURED)).thenReturn(message);
        validator.validateSupplier8DProcessType(null);
    }

    @Test
    public void testValidateEightDProgress() {
        QualityNotifications  qn = Struct.create(QualityNotifications.class);
        qn.setPersonResponsibleId("ABCD");
        List<Row> rowvalues = new ArrayList<>();
        row.put("ID", "ComplaintID");
        opt = Optional.of(row);
        rowvalues.add(row);
        when(eightDDao.getEightDBasedOnId(eightD.getId())).thenReturn(result);
        when(eightDService.getEightDDetails(eightD.getId())).thenReturn(eightD);
        eightD.setStatusCode("S8DCMPL");
        Mockito.when(result.list()).thenReturn(rowvalues);
        Mockito.when(result.first()).thenReturn(opt);
        when(messages.error(MessageKeys.ATTRIBUTES_NOT_EDITABLE)).thenReturn(message);
        when(messages.error(MessageKeys.NOTIFICATION_TYPE_NOT_EDITABLE)).thenReturn(message);
        when(messages.error(MessageKeys.PERSON_RESPONISBLE_NOT_EDITABLE)).thenReturn(message);

        validator.validateFieldControlEightD(eightD);
    }

    @Test
    public void testValidateEightDProgressError() {
        QualityNotifications  qn = Struct.create(QualityNotifications.class);
        qn.setPersonResponsibleId("ABCD");
        List<Row> rowvalues = new ArrayList<>();
        row.put("ID", "ComplaintID");
        opt = Optional.of(row);
        rowvalues.add(row);
        when(eightDDao.getEightDBasedOnId(eightD.getId())).thenReturn(result);
        when(eightDService.getEightDDetails(eightD.getId())).thenReturn(eightD1);
        eightD.setStatusCode("CLMSENT");
        Mockito.when(result.list()).thenReturn(rowvalues);
        Mockito.when(result.first()).thenReturn(opt);
        when(messages.error(MessageKeys.ATTRIBUTES_NOT_EDITABLE)).thenReturn(message);
        when(messages.error(MessageKeys.NOTIFICATION_TYPE_NOT_EDITABLE)).thenReturn(message);

        validator.validateFieldControlEightD(eightD);
    }

    @Test
    public void testValidateIf8DExistsForComplaint() {
        when(eightDService.getEightDDetailsBasedOnComplaintId("1234")).thenReturn(eightD);
        when(messages.error(MessageKeys.SUPPLIER_EIGHTD_EXISTS)).thenReturn(message);
        validator.validateIf8DExistsForComplaint("1234");
    }

    @Test
    public void testValidateIf8DExistsForComplaintForNull() {
        when(eightDService.getEightDDetailsBasedOnComplaintId("1234")).thenReturn(null);
        when(messages.error(MessageKeys.SUPPLIER_EIGHTD_EXISTS)).thenReturn(message);
        validator.validateIf8DExistsForComplaint("1234");
    }

    @Test
    public void testValidateEightDProgressNull() {
        eightD.setSupplierIssueProcessesType(null);
        eightD.setPersonResponsibleId(null);
        validator.validateEightDProgress(eightD, eightD1);
    }

    @Test
    public void testValidateEightDProgressElse() {
        eightD.setSupplierIssueProcessesType("FGHJ");
        eightD.setPersonResponsibleId("asdf");
        eightD1.setSupplierIssueProcessesType("FGHJ");
        eightD1.setPersonResponsibleId("asdf");
        validator.validateEightDProgress(eightD, eightD1);
    }

    @Test
    public void testValidateQNFieldsPONumber(){
        QualityNotifications  qn = Struct.create(QualityNotifications.class);
        qn.setPurchaseOrderItem("10");
        when(messages.error(MessageKeys.PURCHASE_ORDER_NUMBER_MANDATORY)).thenReturn(message);
        validator.validateQNFields(qn);
    }

    @Test
    public void testValidateQNFieldsPOItem(){
        QualityNotifications  qn = Struct.create(QualityNotifications.class);
        qn.setPurchaseOrderNumber("4500000554");
        qn.setPurchaseOrderItem("");
        when(messages.error(MessageKeys.PURCHASE_ORDER_ITEM_NUMBER_MANDATORY)).thenReturn(message);
        validator.validateQNFields(qn);
    }

}
