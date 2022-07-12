package com.sap.ic.cmh.complaint.validation;

import cds.gen.complaintservice.Complaints;
import cds.gen.masterdataservice.BusinessPartners;
import cds.gen.masterdataservice.CompanyCodes;
import cds.gen.masterdataservice.MaterialMasterGeneralDatas;
import cds.gen.masterdataservice.PurchaseOrganizations;
import com.sap.ic.cmh.configuration.validations.ConfigurationFieldsValidation;
import com.sap.cds.Struct;
import com.sap.cds.services.messages.Message;
import com.sap.cds.services.messages.Messages;
import com.sap.ic.cmh.complaint.service.ComplaintService;
import com.sap.ic.cmh.configuration.validations.MasterDataValidation;
import com.sap.ic.cmh.gen.MessageKeys;
import com.sap.ic.cmh.masterdata.common.service.MasterDataService;
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

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

public class ComplaintValidationImplTest {

	@InjectMocks
	@Autowired
	ComplaintValidationImpl complaintValidator;
	@Mock
	MasterDataService masterDataService;
	@Mock
	ComplaintService complaintService;
	@Mock
	Messages messages;
	@Mock
	Message message1;
	@Mock
	MasterDataValidation masterDataValidation;
    @Mock
    ConfigurationFieldsValidation configurationFieldsValidation;
	@Mock
	SecurityValidator securityValidator;
	Complaints complaint;
	private List<Complaints> complaintLists = new ArrayList<>();

	@Before
	public void setup() {
		MockitoAnnotations.openMocks(this);
		complaint = Struct.create(Complaints.class);
		complaint.setId("ComplaintID");
		complaint.setReferenceNumber("1234");
		complaintLists.add(complaint);
	}

	@Test
	public void testValidateComplaint() {
		complaint.setPlantId("1000");
		complaint.setCompanyCodeId("1000");
		complaint.setCurrencyCode("EUR");
		complaint.setPersonResponsibleId("person1");
        complaint.setQuantity(new BigDecimal(20));
		when(messages.error(any(String.class),any(Object[].class))).thenReturn(message1);
        when(message1.target(any(String.class))).thenReturn(message1);
		when(messages.error(MessageKeys.COMPLAINT_ALREADY_EXISTS)).thenReturn(message1);
		when(complaintService.getComplaintDetails(complaint.getId())).thenReturn(complaint);
        when(configurationFieldsValidation.isValidateNumericValue(any())).thenReturn(false);
        when(messages.error(MessageKeys.NUMBER_IS_NEGATIVE)).thenReturn(message1);
		complaintValidator.validateComplaintBeforeCreate(complaint);

	}

	@Test
	public void testValidateComplaintNull() {
		when(complaintService.getComplaintDetails(complaint.getId())).thenReturn(null);
		complaintValidator.validateComplaintBeforeCreate(complaint);
	}

	@Test
	public void testValidateComplaintNull2() {
		BusinessPartners bp = Struct.create(BusinessPartners.class);
		bp.setIsMarkedForDeletion(true);
		complaint.setSupplierId("supID");
		when(complaintService.getComplaintDetails(complaint.getId())).thenReturn(null);
		when(masterDataService.getSupplier(any(String.class))).thenReturn(bp);
		when(messages.error(MessageKeys.BUSINESS_PARTNER_MARKED_FOR_DELETION)).thenReturn(message1);
		complaintValidator.validateComplaintBeforeCreate(complaint);
	}

	@Test
	public void testValidateComplaintNull3() {
		BusinessPartners bp = Struct.create(BusinessPartners.class);
		bp.setIsMarkedForDeletion(true);
		when(complaintService.getComplaintDetails(complaint.getId())).thenReturn(null);
		when(masterDataService.getSupplier(any(String.class))).thenReturn(bp);
		when(messages.error(MessageKeys.BUSINESS_PARTNER_MARKED_FOR_DELETION)).thenReturn(message1);
		complaintValidator.validateComplaintBeforeCreate(complaint);
	}

	@Test
	public void testValidateComplaintGetIsMarkedForDeletionFalse() {
		BusinessPartners bp = Struct.create(BusinessPartners.class);
		bp.setIsMarkedForDeletion(false);
		bp.setBusinessPartnerType("person1");
		complaint.setSupplierId("ifg");
		when(complaintService.getComplaintDetails("ComplaintID")).thenReturn(null);
		when(masterDataService.getSupplier(any(String.class))).thenReturn(bp);
		when(messages.error(MessageKeys.WRONG_SUPPLIER_SELECTED)).thenReturn(message1);
		complaintValidator.validateComplaintBeforeCreate(complaint);

	}

	@Test
	public void testValidateComplaintBusinessPartnerTypeSUP() {
		BusinessPartners bp = Struct.create(BusinessPartners.class);
		bp.setIsMarkedForDeletion(false);
		bp.setBusinessPartnerType("SUP");
		complaint.setSupplierId("ifg");
		when(complaintService.getComplaintDetails("ComplaintID")).thenReturn(null);
		when(masterDataService.getSupplier(any(String.class))).thenReturn(bp);
		when(messages.error(MessageKeys.WRONG_SUPPLIER_SELECTED)).thenReturn(message1);
		complaintValidator.validateComplaintBeforeCreate(complaint);

	}

	@Test
	public void testValidateComplaintBySupplierId() {
		complaint.setSupplierId("F908834");
		when(messages.error(MessageKeys.INVALID_SUPPLIER)).thenReturn(message1);
		when(complaintService.getComplaintDetails(complaint.getId())).thenReturn(null);
		complaintValidator.validateComplaintBeforeCreate(complaint);

	}

	@Test
	public void testValidateComplaintByPlant() {
		complaint.setPlantId("E456");
		when(messages.error(MessageKeys.INVALID_PLANT)).thenReturn(message1);
		when(complaintService.getComplaintDetails(complaint.getId())).thenReturn(null);
		complaintValidator.validateComplaintBeforeCreate(complaint);

	}

	@Test
	public void testValidateComplaintByCompanyCode() {
		complaint.setCompanyCodeId("T5677");
		when(messages.error(MessageKeys.INVALID_COMPANY)).thenReturn(message1);
		when(complaintService.getComplaintDetails(complaint.getId())).thenReturn(null);
		complaintValidator.validateComplaintBeforeCreate(complaint);

	}

	@Test
	public void testValidateComplaintByCompanyCodeNotNull() {
		CompanyCodes code = Struct.create(CompanyCodes.class);
		code.setCompanyCode("G001");
		complaint.setCompanyCodeId("T5677");
		complaint.setPlantId("F001");
		when(masterDataService.getCompanyCode(any(String.class))).thenReturn(code);
		when(masterDataService.validateComplanyCodeBasedOnPlant(code.getCompanyCode())).thenReturn(null);
		when(messages.error(MessageKeys.INVALID_COMPANY)).thenReturn(message1);
		when(messages.error(MessageKeys.INVALID_PLANT)).thenReturn(message1);
		when(complaintService.getComplaintDetails(complaint.getId())).thenReturn(null);
		complaintValidator.validateComplaintBeforeCreate(complaint);

	}

	@Test
	public void testValidateComplaintByPurchasingOrganizationId() {
		PurchaseOrganizations purchaseOrganizations=Struct.create(PurchaseOrganizations.class);
		complaint.setPurchasingOrganizationId("I890");
		when(masterDataService.getPurchasingOrganization(complaint.getPurchasingOrganizationId())).thenReturn(purchaseOrganizations);
		when(messages.error(MessageKeys.INVALID_PURCHASING_ORGANIZATION)).thenReturn(message1);
		when(complaintService.getComplaintDetails(complaint.getId())).thenReturn(null);
		complaintValidator.validateComplaintBeforeCreate(complaint);

	}

	@Test
	public void testValidateComplaintByMaterialId() {
		MaterialMasterGeneralDatas materialMasterGeneralDatas=Struct.create(MaterialMasterGeneralDatas.class);
		complaint.setMaterialId("I890");
		when(masterDataService.getMaterial(complaint.getMaterialId())).thenReturn(materialMasterGeneralDatas);
		when(messages.error(MessageKeys.INVALID_MATERIAL)).thenReturn(message1);
		when(complaintService.getComplaintDetails(complaint.getId())).thenReturn(null);
		complaintValidator.validateComplaintBeforeCreate(complaint);

	}

	@Test
	public void validateComplaintFreeTextFieldsTest() {
		complaint.setSupplierId("I890");
		complaint.setPlantId("20");
		complaint.setDescription("test");
		complaint.setNote("test");
		Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(message1);
		Mockito.when(message1.target(any(String.class))).thenReturn(message1);
		complaintValidator.validateComplaintFreeTextFields(complaint);
	}

	@Test
	public void testValidateComplaintFreeTextFields() {
		complaint.setSupplierId("I890");
		complaint.setPlantId("20");
		complaint.setDescription("te@#$$%st");
		complaint.setNote("te@#$$%st");
		when(messages.error(any(String.class), any(Object[].class))).thenReturn(message1);
		when(message1.target(any(String.class))).thenReturn(message1);
		when(securityValidator.isValidText(complaint.getDescription())).thenReturn(true);
		complaintValidator.validateComplaintFreeTextFields(complaint);
	}

	@Test
	public void testValidateComplaintAttributes() {
		complaint.setId("998764");
		complaint.setSupplierId("1111");
		complaint.setMaterialId("2222");
		complaint.setPlantId("3333");
		complaint.setQuantity(new BigDecimal(1));
		complaint.setUnitCode("ST");
		complaint.setPurchasingOrganizationId("4444");
		complaint.setCompanyCodeId("5555");
		complaint.setComplaintStatusCode("INPR");
		Complaints complaintInDb= Struct.create(Complaints.class);
		complaintInDb.setId("998764");
		complaintInDb.setSupplierId("223343");
		complaintInDb.setMaterialId("8676");
		complaintInDb.setPlantId("0897");
		complaintInDb.setQuantity(new BigDecimal(1));
		complaintInDb.setUnitCode("ST");
		complaintInDb.setPurchasingOrganizationId("4444");
		complaintInDb.setCompanyCodeId("5555");
		complaintInDb.setComplaintStatusCode("INPR");
		when(complaintService.getComplaintDetails("998764")).thenReturn(complaintInDb);
		Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(message1);
		complaintValidator.validateComplaintAttributes(complaint);
	}

	@Test
	public void testValidateComplaintAttributesNull() {
		complaint.setId("998764");
		complaint.setSupplierId("1111");
		complaint.setMaterialId("2222");
		complaint.setPlantId("3333");
		complaint.setQuantity(new BigDecimal(1));
		complaint.setUnitCode("ST");
		complaint.setPurchasingOrganizationId("4444");
		complaint.setCompanyCodeId("5555");
		complaint.setComplaintStatusCode("INPR");
		Complaints complaintInDb= Struct.create(Complaints.class);
		complaintInDb.setId("998764");
		complaintInDb.setSupplierId("223343");
		complaintInDb.setMaterialId("8676");
		complaintInDb.setPlantId("0897");
		complaintInDb.setQuantity(new BigDecimal(1));
		complaintInDb.setUnitCode("ST");
		complaintInDb.setPurchasingOrganizationId("4444");
		complaintInDb.setCompanyCodeId("5555");
		complaintInDb.setComplaintStatusCode(null);
		when(complaintService.getComplaintDetails("998764")).thenReturn(null);
		Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(message1);
		complaintValidator.validateComplaintAttributes(complaint);
	}

	@Test
	public void testValidateComplaintAttributesElseNull() {
		complaint.setId("998764");
		complaint.setSupplierId("1111");
		complaint.setMaterialId("2222");
		complaint.setPlantId("3333");
		complaint.setQuantity(new BigDecimal(1));
		complaint.setUnitCode("ST");
		complaint.setPurchasingOrganizationId("4444");
		complaint.setCompanyCodeId("5555");
		complaint.setComplaintStatusCode("INPR");
		Complaints complaintInDb= Struct.create(Complaints.class);
		complaintInDb.setId("998764");
		complaintInDb.setSupplierId("223343");
		complaintInDb.setMaterialId("8676");
		complaintInDb.setPlantId("0897");
		complaintInDb.setQuantity(new BigDecimal(1));
		complaintInDb.setUnitCode("ST");
		complaintInDb.setPurchasingOrganizationId("4444");
		complaintInDb.setCompanyCodeId("5555");
		complaintInDb.setComplaintStatusCode(null);
		when(complaintService.getComplaintDetails("998764")).thenReturn(null);
		Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(message1);
		complaintValidator.validateComplaintAttributes(complaint);
	}
	@Test
	public void testValidateComplaintAttributesClosed() {
		complaint.setId("998764");
		complaint.setComplaintStatusCode("CLSD");
		Complaints complaintInDb= Struct.create(Complaints.class);
		complaintInDb.setId("998764");
		complaintInDb.setSupplierId("223343");
		complaintInDb.setMaterialId("8676");
		complaintInDb.setPlantId("0897");
		complaintInDb.setQuantity(new BigDecimal(1));
		complaintInDb.setUnitCode("ST");
		complaintInDb.setPurchasingOrganizationId("4444");
		complaintInDb.setCompanyCodeId("5555");
		complaintInDb.setComplaintStatusCode("CLSD");
		when(complaintService.getComplaintDetails("998764")).thenReturn(complaintInDb);
		Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(message1);
		complaintValidator.validateComplaintAttributes(complaint);
	}

	@Test
	public void testValidateComplaintAttributesStatusCodeNone() {
		complaint.setId("998764");
		complaint.setComplaintStatusCode("None");
		Complaints complaintInDb= Struct.create(Complaints.class);
		complaintInDb.setId("998764");
		complaintInDb.setSupplierId("223343");
		complaintInDb.setMaterialId("8676");
		complaintInDb.setPlantId("0897");
		complaintInDb.setQuantity(new BigDecimal(1));
		complaintInDb.setUnitCode("ST");
		complaintInDb.setPurchasingOrganizationId("4444");
		complaintInDb.setCompanyCodeId("5555");
		complaintInDb.setComplaintStatusCode("CLSD");
		when(complaintService.getComplaintDetails("998764")).thenReturn(complaintInDb);
		when(messages.error(any(String.class), any(Object[].class))).thenReturn(message1);
		complaintValidator.validateComplaintAttributes(complaint);
	}

	@Test
	public void testValidateComplaintAttributesComplaintInDbNull() {
		complaint.setId("998764");
		complaint.setComplaintStatusCode("");
		when(complaintService.getComplaintDetails("998764")).thenReturn(null);
		Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(message1);
		complaintValidator.validateComplaintAttributes(complaint);
	}

	@Test
	public void validateComplaintFieldControlTest() {
		complaint.setSupplierId("22334");
		Complaints complaintInDb= Struct.create(Complaints.class);
		complaintInDb.setSupplierId("223343");
		Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(message1);
		Mockito.when(message1.target(any(String.class))).thenReturn(message1);
		complaintValidator.validateComplaintFieldControl(complaint,complaintInDb);
	}
	@Test
	public void validateComplaintFieldControlNullTest() {
		complaint.setSupplierId(null);
		Complaints complaintInDb= Struct.create(Complaints.class);
		complaintInDb.setSupplierId(null);
		Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(message1);
		Mockito.when(message1.target(any(String.class))).thenReturn(message1);
		complaintValidator.validateComplaintFieldControl(complaint,complaintInDb);
	}

	@Test
	public void validateComplaintFieldControlElseTest() {
		complaint.setMaterialId("867");
		Complaints complaintInDb= Struct.create(Complaints.class);
		complaintInDb.setMaterialId("8676");
		Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(message1);
		Mockito.when(message1.target(any(String.class))).thenReturn(message1);
		complaintValidator.validateComplaintFieldControl(complaint,complaintInDb);
	}
	@Test
	public void validateComplaintFieldControlElseNullTest() {
		complaint.setMaterialId(null);
		Complaints complaintInDb= Struct.create(Complaints.class);
		complaintInDb.setMaterialId(null);
		Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(message1);
		Mockito.when(message1.target(any(String.class))).thenReturn(message1);
		complaintValidator.validateComplaintFieldControl(complaint,complaintInDb);
	}
	@Test
	public void validateComplaintFieldControlElse1Test() {
		complaint.setQuantity(new BigDecimal(0));
		Complaints complaintInDb= Struct.create(Complaints.class);
		complaintInDb.setQuantity(new BigDecimal(1));
		Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(message1);
		Mockito.when(message1.target(any(String.class))).thenReturn(message1);
		complaintValidator.validateComplaintFieldControl(complaint,complaintInDb);
	}

	@Test
	public void validateComplaintFieldControlElse1NullTest() {
		complaint.setQuantity(null);
		Complaints complaintInDb= Struct.create(Complaints.class);
		complaintInDb.setQuantity(null);
		Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(message1);
		Mockito.when(message1.target(any(String.class))).thenReturn(message1);
		complaintValidator.validateComplaintFieldControl(complaint,complaintInDb);
	}
	@Test
	public void validateComplaintFieldControlElse2Test() {
		complaint.setUnitCode("STT");
		Complaints complaintInDb= Struct.create(Complaints.class);
		complaintInDb.setUnitCode("ST");
		Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(message1);
		Mockito.when(message1.target(any(String.class))).thenReturn(message1);
		complaintValidator.validateComplaintFieldControl(complaint,complaintInDb);
	}

	@Test
	public void validateComplaintFieldControlElse2NullTest() {
		complaint.setUnitCode(null);
		Complaints complaintInDb= Struct.create(Complaints.class);
		complaintInDb.setUnitCode(null);
		Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(message1);
		Mockito.when(message1.target(any(String.class))).thenReturn(message1);
		complaintValidator.validateComplaintFieldControl(complaint,complaintInDb);
	}
	@Test
	public void validateComplaintFieldControlElse3Test() {
		complaint.setPlantId("089");
		Complaints complaintInDb= Struct.create(Complaints.class);
		complaintInDb.setPlantId("0897");
		Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(message1);
		Mockito.when(message1.target(any(String.class))).thenReturn(message1);
		complaintValidator.validateComplaintFieldControl(complaint,complaintInDb);
	}
	@Test
	public void validateComplaintFieldControlElse3NullTest() {
		complaint.setPlantId(null);
		Complaints complaintInDb= Struct.create(Complaints.class);
		complaintInDb.setPlantId(null);
		Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(message1);
		Mockito.when(message1.target(any(String.class))).thenReturn(message1);
		complaintValidator.validateComplaintFieldControl(complaint,complaintInDb);
	}
	@Test
	public void validateComplaintFieldControlElse4Test() {
		complaint.setPurchasingOrganizationId("444");
		Complaints complaintInDb= Struct.create(Complaints.class);
		complaintInDb.setPurchasingOrganizationId("4444");
		Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(message1);
		Mockito.when(message1.target(any(String.class))).thenReturn(message1);
		complaintValidator.validateComplaintFieldControl(complaint,complaintInDb);
	}

	@Test
	public void validateComplaintFieldControlElse4NullTest() {
		complaint.setPurchasingOrganizationId(null);
		Complaints complaintInDb= Struct.create(Complaints.class);
		complaintInDb.setPurchasingOrganizationId(null);
		Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(message1);
		Mockito.when(message1.target(any(String.class))).thenReturn(message1);
		complaintValidator.validateComplaintFieldControl(complaint,complaintInDb);
	}

	@Test
	public void validateComplaintFieldControlNull() {
		Complaints complaintInDb = Struct.create(Complaints.class);
		Complaints complaint = Struct.create(Complaints.class);
		complaintInDb.setId("12");
		complaint.setId("12");
		complaintInDb.setSupplierId(null);
		complaintInDb.setMaterialId(null);
		complaintInDb.setPlantId(null);
		complaintInDb.setPurchasingOrganizationId(null);
		complaintInDb.setQuantity(null);
		complaintInDb.setUnitCode(null);
		complaint.setSupplierId(null);
		complaint.setMaterialId(null);
		complaint.setPlantId(null);
		complaint.setPurchasingOrganizationId(null);
		complaint.setQuantity(null);
		complaint.setUnitCode(null);
		complaintValidator.validateComplaintFieldControl(complaint,complaintInDb);
	}

	@Test
	public void validateComplaintFieldControlSame() {
		Complaints complaintInDb = Struct.create(Complaints.class);
		Complaints complaint = Struct.create(Complaints.class);
		complaintInDb.setId("12");
		complaint.setId("12");
		complaintInDb.setSupplierId("abc");
		complaintInDb.setMaterialId("abc");
		complaintInDb.setPlantId("abc");
		complaintInDb.setPurchasingOrganizationId("abc");
		complaintInDb.setQuantity(BigDecimal.TEN);
		complaintInDb.setUnitCode("abc");
		complaint.setSupplierId("abc");
		complaint.setMaterialId("abc");
		complaint.setPlantId("abc");
		complaint.setPurchasingOrganizationId("abc");
		complaint.setQuantity(BigDecimal.TEN);
		complaint.setUnitCode("abc");
		complaintValidator.validateComplaintFieldControl(complaint,complaintInDb);
	}

	@Test
	public void testValidatePlant() {
		complaintValidator.validatePlant("1234");
	}

	@Test
	public void testValidateResponsiblePerson() {
		when(masterDataValidation.validateBTPUser(any())).thenReturn(true);
		complaintValidator.validateResponsiblePerson("Alexander");
	}


	@Test
	public void testValidateResponsiblePersonIsEmpty() {
		when(masterDataValidation.validateBTPUser(any())).thenReturn(false);
		when(messages.error(MessageKeys.INVALID_RESPONSIBLE_PERSON)).thenReturn(message1);
		complaintValidator.validateResponsiblePerson("Alexander");
	}

	@Test
	public void testValidateMasterDataDeletion() {
		complaint.setSupplierId(null);
		complaintValidator.validateMasterDataDeletion(complaint);
	}
	@Test
	public void testValidateMasterDataDeletionComplaintsNull() {
		Complaints complaint1 = Struct.create(Complaints.class);
		complaintValidator.validateMasterDataDeletion(complaint);
	}

	@Test
	public void testValidateComplaintAttributesStatusCodeBlank() {
		complaint.setComplaintStatusCode("");
		Complaints complaint1 = Struct.create(Complaints.class);
		when(complaintService.getComplaintDetails(any())).thenReturn(complaint1);
		complaintValidator.validateMasterDataDeletion(complaint);
	}

    @Test
	public void validateComplaintFieldControlRefNumberTest() {
		complaint.setReferenceNumber("REF");
		Complaints complaintInDb= Struct.create(Complaints.class);
		complaintInDb.setReferenceNumber("REFF1");
		Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(message1);
		Mockito.when(message1.target(any(String.class))).thenReturn(message1);
		complaintValidator.validateComplaintFieldControl(complaint,complaintInDb);
	}
	@Test
	public void validateComplaintFieldControlRefNumberNullTest() {
		complaint.setReferenceNumber(null);
		Complaints complaintInDb= Struct.create(Complaints.class);
		complaintInDb.setReferenceNumber(null);
		Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(message1);
		Mockito.when(message1.target(any(String.class))).thenReturn(message1);
		complaintValidator.validateComplaintFieldControl(complaint,complaintInDb);
	}

    
}
