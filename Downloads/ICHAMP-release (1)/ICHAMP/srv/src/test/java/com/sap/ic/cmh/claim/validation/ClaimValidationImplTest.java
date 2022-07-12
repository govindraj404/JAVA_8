package com.sap.ic.cmh.claim.validation;

import cds.gen.claimservice.Claims;
import cds.gen.complaintservice.Actions;
import cds.gen.masterdataservice.BusinessPartners;
import cds.gen.qualitynotificationservice.QualityNotifications;
import com.sap.cds.Result;
import com.sap.cds.Row;
import com.sap.cds.Struct;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.messages.Message;
import com.sap.cds.services.messages.Messages;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.ic.cmh.action.persistency.ActionDao;
import com.sap.ic.cmh.businessobjects.service.BusinessObjectService;
import com.sap.ic.cmh.claim.service.ClaimService;
import com.sap.ic.cmh.claim.validations.ClaimValidationImpl;
import com.sap.ic.cmh.configuration.persistency.ConfigurationDao;
import com.sap.ic.cmh.configuration.validations.MasterDataValidation;
import com.sap.ic.cmh.gen.MessageKeys;
import com.sap.ic.cmh.qualitynotification.service.QualityNotificationService;
import com.sap.ic.cmh.utils.Constants;
import com.sap.ic.cmh.utils.SecurityValidator;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;

import java.math.BigDecimal;
import java.util.*;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

public class ClaimValidationImplTest {

    @InjectMocks
    @Autowired
    ClaimValidationImpl claimValidator;

    @Mock
    Messages message;

    @Mock
    Message message1;

    @Mock
    SecurityValidator securityValidator;

    @Mock
    MasterDataValidation masterDataValidation;

    @Mock
    PersistenceService mockDb;

    @Mock
    ClaimService claimService;
    @Mock
    BusinessObjectService businessObjectService;
    @Mock
    QualityNotificationService qualityNotificationService;
    @Mock
    ActionDao actionDao;
    @Mock
    Result result;

    @Mock
    ConfigurationDao configurationDao;

    private Claims claim;
    private Claims claim1;
    private Claims claim2;
    private QualityNotifications qn;
    private Row row;
    private Optional<Row> opt;

    private BusinessPartners partner;
    private List<BusinessPartners> partnerList = new ArrayList<>();


    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
        partner = Struct.create(BusinessPartners.class);
        partner.setBusinessPartnerNumber("1234");
        partner.setIsMarkedForDeletion(true);
        partnerList.add(partner);

        claim = Struct.create(Claims.class);
        claim.setId("01");
        claim.setItemTypeCode("I01");
        claim.setClaimType("C01");
        claim.setSupplierRole("Manager");
        claim.setVersionCategory("101");
        claim.setPersonResponsibleId("1234");
        Map<String, String> map1 = new HashMap<>();
        map1.put("01", "01");
        claim.setPersonResponsible(map1);


        claim1 = Struct.create(Claims.class);
        claim1.setItemTypeCode(null);
        claim1.setClaimType(null);
        claim1.setSupplierRole(null);
        claim1.setVersionCategory(null);


        claim2 = Struct.create(Claims.class);
        claim2.setId("01");
        claim2.setItemTypeCode("*^^^*");
        claim2.setClaimType("C01");
        claim2.setSupplierRole("900");
        claim2.setVersionCategory("EC");
        claim2.setStatusCode("NEW");
        claim2.setId("01");
        Map<String, String> map = new HashMap<>();
        map.put("01", "01");
        claim2.setPersonResponsible(map);

        qn = Struct.create(QualityNotifications.class);
        qn.setComplaintId("F677");
        qn.setId("345");
        qn.setQuantity(BigDecimal.TEN);
        qn.setUnit("GTY");
        qn.setPersonResponsibleId("w34");
        qn.setCompanyId("companyId");
        qn.setStatusCode("QNINPR");

        row = Struct.create(Row.class);

    }

    @Test
    public void testValidateClaimFields() {
        when(message.error(MessageKeys.INVALID_TYPE_FOR_CLAIM)).thenReturn(message1);
        when(message.error(MessageKeys.INVALID_ITEM_TYPE_FOR_CLAIM)).thenReturn(message1);
        when(message.error(MessageKeys.INVALID_SUPPLIER_ROLE_FOR_CLAIM)).thenReturn(message1);
        when(message.error(MessageKeys.INVALID_VERSION_CATEGORY_FOR_CLAIM)).thenReturn(message1);
        when(configurationDao.getSupplierData("1234")).thenReturn(result);
        when(message.error(MessageKeys.PERS_RESP_IS_NOT_VALID)).thenReturn(message1);
        claimValidator.validateClaimFields(claim);

    }

    @Test
    public void testValidateClaimFieldsNotNullPartnerTrue() {
        when(message.error(MessageKeys.INVALID_TYPE_FOR_CLAIM)).thenReturn(message1);
        when(message.error(MessageKeys.INVALID_ITEM_TYPE_FOR_CLAIM)).thenReturn(message1);
        when(message.error(MessageKeys.INVALID_SUPPLIER_ROLE_FOR_CLAIM)).thenReturn(message1);
        when(message.error(MessageKeys.INVALID_VERSION_CATEGORY_FOR_CLAIM)).thenReturn(message1);
        when(configurationDao.getSupplierData("1234")).thenReturn(result);
        opt = Optional.of(row);
        when(result.first()).thenReturn(opt);
        when(result.listOf(BusinessPartners.class)).thenReturn(partnerList);
        when(message.error(MessageKeys.PERSON_RESP_MARKED_FOR_DELETION)).thenReturn(message1);
        claimValidator.validateClaimFields(claim);

    }

    @Test
    public void testValidateClaimFieldsNotNull() {
        partner.setIsMarkedForDeletion(false);
        partner.setBusinessPartnerType("ASDF");
        partnerList.add(partner);
        when(message.error(MessageKeys.INVALID_TYPE_FOR_CLAIM)).thenReturn(message1);
        when(message.error(MessageKeys.INVALID_ITEM_TYPE_FOR_CLAIM)).thenReturn(message1);
        when(message.error(MessageKeys.INVALID_SUPPLIER_ROLE_FOR_CLAIM)).thenReturn(message1);
        when(message.error(MessageKeys.INVALID_VERSION_CATEGORY_FOR_CLAIM)).thenReturn(message1);
        when(configurationDao.getSupplierData("1234")).thenReturn(result);
        opt = Optional.of(row);
        when(result.first()).thenReturn(opt);
        when(result.listOf(BusinessPartners.class)).thenReturn(partnerList);
        when(message.error(MessageKeys.WRONG_PERSON_RESP_SELECTED)).thenReturn(message1);
        claimValidator.validateClaimFields(claim);

    }

    @Test
    public void testValidateClaimFieldsPartnerTypeSame() {
        partner.setIsMarkedForDeletion(false);
        partner.setBusinessPartnerType("PERRES");
        partnerList.add(partner);
        when(message.error(MessageKeys.INVALID_TYPE_FOR_CLAIM)).thenReturn(message1);
        when(message.error(MessageKeys.INVALID_ITEM_TYPE_FOR_CLAIM)).thenReturn(message1);
        when(message.error(MessageKeys.INVALID_SUPPLIER_ROLE_FOR_CLAIM)).thenReturn(message1);
        when(message.error(MessageKeys.INVALID_VERSION_CATEGORY_FOR_CLAIM)).thenReturn(message1);
        when(configurationDao.getSupplierData("1234")).thenReturn(result);
        opt = Optional.of(row);
        when(result.first()).thenReturn(opt);
        when(result.listOf(BusinessPartners.class)).thenReturn(partnerList);
        when(message.error(MessageKeys.PERSON_RESP_MARKED_FOR_DELETION)).thenReturn(message1);
        claimValidator.validateClaimFields(claim);

    }

    @Test
    public void testValidateClaimFieldsNull() {
        when(message.error(MessageKeys.ITEM_TYPE_NOT_CONFIGURED)).thenReturn(message1);
        when(message.error(MessageKeys.CLAIM_TYPE_NOT_CONFIGURED)).thenReturn(message1);
        when(message.error(MessageKeys.INVALID_SUPPLIER_ROLE_FOR_CLAIM)).thenReturn(message1);
        when(message.error(MessageKeys.VERSION_CATEGORY_NOT_CONFIGURED)).thenReturn(message1);
        claimValidator.validateClaimFields(claim1);

    }

    @Test
    public void testValidateClaimFieldsAreValid() {
        when(securityValidator.isValidText(any(String.class))).thenReturn(true);
        when(message.error(MessageKeys.ITEM_TYPE_DO_NOT_EXIST)).thenReturn(message1);
        claimValidator.validateClaimFields(claim2);
    }

    @Test
    public void testValidateFieldControlClaim() {
        claim.setClaimType("C987");
        when(message.error(MessageKeys.NOTIFICATION_TYPE_NOT_EDITABLE)).thenReturn(message1);
        when(claimService.getClaimBasedOnId("01")).thenReturn(claim);
        claimValidator.validateFieldControlClaim(claim2);
    }

    @Test
    public void testValidateFieldControlClaimStatusClosed() {
        claim2.setStatusCode("CLMCLSD");
        when(claimService.getClaimBasedOnId("01")).thenReturn(claim);
        when(message.error(MessageKeys.ATTRIBUTES_NOT_EDITABLE)).thenReturn(message1);
        claimValidator.validateFieldControlClaim(claim2);
    }

    @Test
    public void testValidateFieldControlClaimStatusNew() {
        claim2.setStatusCode("NEW");
        when(claimService.getClaimBasedOnId("01")).thenReturn(claim);
        when(message.error(MessageKeys.ATTRIBUTES_NOT_EDITABLE)).thenReturn(message1);
        claimValidator.validateFieldControlClaim(claim2);
    }

    @Test
    public void testValidateFieldControlClaimStatusNotNew() {
        claim2.setStatusCode("CLMVLTD");
        when(claimService.getClaimBasedOnId(claim.getId())).thenReturn(claim);
        when(message.error(MessageKeys.VERSION_CATEGORY_NOT_EDITABLE)).thenReturn(message1);
        
        claimValidator.validateFieldControlClaim(claim2);
    }

    @Test
    public void testValidateFieldControlClaimSupplierRoleError() {
        claim.getPersonResponsible().setBusinessPartnerNumber("01");
        claim2.getPersonResponsible().setBusinessPartnerNumber("01");
        claim2.setVersionCategory("101");
        when(claimService.getClaimBasedOnId("01")).thenReturn(claim);
        when(message.error(MessageKeys.SUPPLIER_ROLE_NOT_EDITABLE)).thenReturn(message1);
        claimValidator.validateFieldControlClaim(claim2);
    }

    @Test
    public void testValidateFieldControlClaimVCError() {
        claim.getPersonResponsible().setBusinessPartnerNumber("01");
        claim2.getPersonResponsible().setBusinessPartnerNumber("01");
        when(claimService.getClaimBasedOnId("01")).thenReturn(claim);
        when(message.error(MessageKeys.VERSION_CATEGORY_NOT_EDITABLE)).thenReturn(message1);
        claimValidator.validateFieldControlClaim(claim2);
    }

    @Test
    public void testValidateFieldControlClaimPRBPNError() {
        claim.setPersonResponsibleId("01");
        claim2.setPersonResponsibleId("011");
        when(claimService.getClaimBasedOnId("01")).thenReturn(claim);
        when(message.error(MessageKeys.PERSON_RESPONISBLE_NOT_EDITABLE)).thenReturn(message1);
        claimValidator.validateFieldControlClaim(claim2);
    }

    @Test
    public void testValidateFieldControlClaimItemTypeCodeError() {
        claim.getPersonResponsible().setBusinessPartnerNumber("01");
        claim2.getPersonResponsible().setBusinessPartnerNumber("01");
        claim2.setVersionCategory("101");
        claim2.setSupplierRole("manager");
        when(claimService.getClaimBasedOnId("01")).thenReturn(claim);
        when(message.error(MessageKeys.ITEM_TYPE_NOT_EDITABLE)).thenReturn(message1);
        claimValidator.validateFieldControlClaim(claim2);
    }

    @Test
    public void testValidateFieldControlClaimNoIfSatisfied() {
        claim.getPersonResponsible().setBusinessPartnerNumber("01");
        claim2.getPersonResponsible().setBusinessPartnerNumber("01");
        claim2.setVersionCategory("101");
        claim2.setSupplierRole("manager");
        claim2.setItemTypeCode("I01");
        when(claimService.getClaimBasedOnId("01")).thenReturn(claim);
        claimValidator.validateFieldControlClaim(claim2);
    }

    @Test
    public void testValidateIfClaimExistsForComplaint() {
        claim.setComplaintId("COMP001");
        claim2.setComplaintId("COMP002");
        when(claimService.getClaimBasedOnId(null)).thenReturn(null);
        when(claimService.getClaim("COMP001")).thenReturn(claim);
        when(message.error(MessageKeys.CLAIM_EXISTS)).thenReturn(message1);
        claimValidator.validateIfClaimExistsForComplaint(claim2.getComplaintId());
    }

    @Test
    public void testValidateIfClaimExistsForComplaintNotNull() {
        claim.setComplaintId("COMP001");
        claim.setId("23");
        when(claimService.getClaim("COMP001")).thenReturn(claim);
        when(message.error(MessageKeys.CLAIM_EXISTS)).thenReturn(message1);
        claimValidator.validateIfClaimExistsForComplaint(claim.getComplaintId());
    }

    @Test
    public void testValidateIfBOIsRelevantTrue() {
        claim.setComplaintId("COMP001");
        claim2.setComplaintId("COMP001");
        claim.setId("CLAIM01");
        claim2.setId("CLAIM01");
        when(businessObjectService.checkIfBOIsRelevant(any(String.class), any(String.class))).thenReturn(true);
        claimValidator.validateIfBOIsRelevant("COMP001", "CLAIM01");
    }

    @Test
    public void testValidateIfBOIsRelevantFalse() {
        claim.setComplaintId("COMP001");
        claim2.setComplaintId("COMP001");
        claim.setId("CLAIM01");
        claim2.setId("CLAIM01");
        when(businessObjectService.checkIfBOIsRelevant(any(String.class), any(String.class))).thenReturn(false);
        when(message.error(MessageKeys.CLAIM_NOT_RELEVANT)).thenReturn(message1);
        claimValidator.validateIfBOIsRelevant("COMP001", "CLAIM01");
    }

    @Test
    public void testValidateIfClaimExists() {
        claim.setId("CLAIM01");
        claim2.setId("CLAIM01");
        when(claimService.getClaimBasedOnId("CLAIM01")).thenReturn(claim);
        when(message.error(MessageKeys.CLAIM_NOT_EXIST)).thenReturn(message1);
        claimValidator.validateIfClaimExists("CLAIM01");
    }

    @Test
    public void testValidateIfClaimExistsNotNUll() {
        when(claimService.getClaimBasedOnId("CLAIM01")).thenReturn(null);
        when(message.error(MessageKeys.CLAIM_NOT_EXIST)).thenReturn(message1);
        claimValidator.validateIfClaimExists("CLAIM01");
    }

    @Test
    public void testValidateIfQualityNotificationExists() {
        List<Row> rowvalues = new ArrayList<>();
        row.put("businessObjectStatus_code", "QNINPR");
        opt = Optional.of(row);
        rowvalues.add(row);

        when(qualityNotificationService.getQualityNotificationDetailsByComplaintId(qn.getComplaintId())).thenReturn(qn);
        when(message.error(MessageKeys.QUALITY_NOTIFICATION_TO_CREATE)).thenReturn(message1);
        qualityNotificationService.getQualityNotificationDetailsByComplaintId(qn.getComplaintId());
        when(message.error(MessageKeys.ACTION_PRE_CONDITION_NOT_SATISFIED)).thenReturn(message1);
        when(actionDao.getActions(Constants.CLAIM_CODE)).thenReturn(result);
        when(result.list()).thenReturn(rowvalues);
        when(result.first()).thenReturn(opt);
        when(businessObjectService.checkIfBOStatusExists("345", "QNINPR")).thenReturn(true);
        claimValidator.validateIfQualityNotificationExists(qn.getComplaintId(), Constants.CLAIM_CODE);
    }

    @Test
    public void testValidateIfQualityNotificationExistsBooleanFalse() {
        List<Row> rowvalues = new ArrayList<>();
        row.put("businessObjectStatus_code", "QNINPR");
        opt = Optional.of(row);
        rowvalues.add(row);

        when(qualityNotificationService.getQualityNotificationDetailsByComplaintId(qn.getComplaintId())).thenReturn(qn);
        when(message.error(MessageKeys.QUALITY_NOTIFICATION_TO_CREATE)).thenReturn(message1);
        qualityNotificationService.getQualityNotificationDetailsByComplaintId(qn.getComplaintId());
        when(message.error(MessageKeys.ACTION_PRE_CONDITION_NOT_SATISFIED)).thenReturn(message1);
        when(actionDao.getActions(Constants.CLAIM_CODE)).thenReturn(result);
        when(result.list()).thenReturn(rowvalues);
        when(result.first()).thenReturn(opt);
        when(businessObjectService.checkIfBOStatusExists("345", "QNINPR")).thenReturn(false);
        claimValidator.validateIfQualityNotificationExists(qn.getComplaintId(), Constants.CLAIM_CODE);
    }

    @Test
    public void testValidateIfQualityNotificationExistsNull() {
        when(qualityNotificationService.getQualityNotificationDetailsByComplaintId(qn.getComplaintId())).thenReturn(null);
        when(message.error(MessageKeys.QUALITY_NOTIFICATION_TO_CREATE)).thenReturn(message1);
        claimValidator.validateIfQualityNotificationExists(qn.getComplaintId(), Constants.CLAIM_CODE);
    }


    @Test
     public void validateItemTypeTest(){
        Mockito.when(message.error(any(String.class),any(Object[].class))).thenReturn(message1);
        Mockito.when(message1.target(any(String.class))).thenReturn(message1);
        claimValidator.validateItemType("test");
    }

    @Test
    public void validateItemTypeElseTest(){
        Mockito.when(message.error(any(String.class),any(Object[].class))).thenReturn(message1);
        Mockito.when(message1.target(any(String.class))).thenReturn(message1);
        claimValidator.validateItemType("");
    }

    @Test
    public void validateItemTypeElse1Test(){
        Mockito.when(message.error(any(String.class),any(Object[].class))).thenReturn(message1);
        Mockito.when(message1.target(any(String.class))).thenReturn(message1);
        claimValidator.validateItemType(null);
    }
    @Test
    public void validateClaimTypeElse1Test(){
        Mockito.when(message.error(any(String.class),any(Object[].class))).thenReturn(message1);
        Mockito.when(message1.target(any(String.class))).thenReturn(message1);
        claimValidator.validateClaimType(null);
    }
    @Test
    public void validateClaimTypeElseTest(){
        Mockito.when(message.error(any(String.class),any(Object[].class))).thenReturn(message1);
        Mockito.when(message1.target(any(String.class))).thenReturn(message1);
        claimValidator.validateClaimType("test");
    }

    @Test
    public void validateClaimTypeTest(){
        Mockito.when(message.error(any(String.class),any(Object[].class))).thenReturn(message1);
        Mockito.when(message1.target(any(String.class))).thenReturn(message1);
        claimValidator.validateClaimType("");
    }

    @Test
    public void validateSupplierRoleTest(){
        Mockito.when(message.error(any(String.class),any(Object[].class))).thenReturn(message1);
        Mockito.when(message1.target(any(String.class))).thenReturn(message1);
        claimValidator.validateSupplierRole("test");
    }

    @Test
    public void validateSupplierRoleEmptyTest(){
        Mockito.when(message.error(any(String.class),any(Object[].class))).thenReturn(message1);
        Mockito.when(message1.target(any(String.class))).thenReturn(message1);
        claimValidator.validateSupplierRole("");
    }
    @Test
    public void validateSupplierRoleNullTest(){
        Mockito.when(message.error(any(String.class),any(Object[].class))).thenReturn(message1);
        Mockito.when(message1.target(any(String.class))).thenReturn(message1);
        claimValidator.validateSupplierRole(null);
    }
    @Test
    public void validateVersionCategoryTest(){
        Mockito.when(message.error(any(String.class),any(Object[].class))).thenReturn(message1);
        Mockito.when(message1.target(any(String.class))).thenReturn(message1);
        claimValidator.validateVersionCategory("test");
    }
    @Test
    public void validateVersionCategoryEmptyTest(){
        Mockito.when(message.error(any(String.class),any(Object[].class))).thenReturn(message1);
        Mockito.when(message1.target(any(String.class))).thenReturn(message1);
        claimValidator.validateVersionCategory("");
    }
    @Test
    public void validateVersionCategoryNullTest(){
        Mockito.when(message.error(any(String.class),any(Object[].class))).thenReturn(message1);
        Mockito.when(message1.target(any(String.class))).thenReturn(message1);
        claimValidator.validateVersionCategory(null);
    }

    @Test
    public void validateActionPreConditionTest(){
        when(actionDao.getActions(MessageKeys.INVALID_SUPPLIER_CONTACT_PERSON_FOR_CLAIM)).thenReturn(result);
        claimValidator.validateActionPreCondition("test","test");
    }
    @Test
    public void validateActionPreConditionExpTest(){
        when(actionDao.getActions(MessageKeys.INVALID_SUPPLIER_CONTACT_PERSON_FOR_CLAIM)).thenReturn(null);
        claimValidator.validateActionPreCondition("test","test");
    }

    @Test
    public void testValidateFieldControlClaimElse() {
        when(claimService.getClaimBasedOnId("01")).thenReturn(null);
        claimValidator.validateFieldControlClaim(claim2);
    }
    @Test
    public void validateActionPreConditionNull(){
         when(actionDao.getActions(any())).thenReturn(result);
        claimValidator.validateActionPreCondition("test","test");
    }



}