package com.sap.ic.cmh.complaint.handler;

import cds.gen.claimservice.Claims;
import cds.gen.complaintservice.*;
import cds.gen.complaintservice.BTPUsers;
import cds.gen.masterdataservice.CompanyCodes;
import cds.gen.masterdataservice.Plants;
import cds.gen.qualitynotificationservice.QualityNotifications;
import cds.gen.complaintservice.QualityNotification;
import cds.gen.complaintservice.QualityNotification_;
import cds.gen.complaintservice.Claim;
import cds.gen.complaintservice.Claim_;
import cds.gen.complaintservice.Supplier8DProcess;
import cds.gen.complaintservice.Supplier8DProcess_;
import cds.gen.complaintservice.ReturnPurchaseOrder;
import cds.gen.complaintservice.ReturnPurchaseOrder_;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.Result;
import com.sap.cds.Row;
import com.sap.cds.Struct;
import com.sap.cds.ql.cqn.CqnInsert;
import com.sap.cds.services.auditlog.Action;
import com.sap.cds.services.cds.CdsReadEventContext;
import com.sap.cds.services.messages.Messages;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.cds.services.request.ParameterInfo;
import com.sap.cds.services.request.UserInfo;
import com.sap.ic.cmh.auditlog.AuditLogDifference;
import com.sap.ic.cmh.auditlog.AuditLogHelper;
import com.sap.ic.cmh.claim.service.ClaimService;
import com.sap.ic.cmh.complaint.service.ComplaintService;
import com.sap.ic.cmh.complaint.service.StreamService;
import com.sap.ic.cmh.complaint.validation.ComplaintValidation;
import com.sap.ic.cmh.masterdata.common.service.MasterDataService;
import com.sap.ic.cmh.qualitynotification.service.QualityNotificationService;
import com.sap.ic.cmh.utils.CommonFunctions;
import com.sap.ic.cmh.utils.Constants;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;
import com.sap.cloud.sdk.cloudplatform.connectivity.ScpCfDestination;
import com.sap.cloud.sdk.cloudplatform.connectivity.ScpCfDestinationLoader;
import com.sap.cloud.sdk.cloudplatform.connectivity.HttpDestination;
import java.util.Iterator;
import com.sap.ic.cmh.configuration.persistency.DestinationConfigurationDao;
import com.sap.ic.cmh.network.service.DestinationService;
import java.net.URI;
import java.math.BigDecimal;
import java.util.*;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import com.sap.ic.cmh.supplierissueprocess.service.EightDService;

public class ComplaintHandlerTest {

	@InjectMocks
	@Autowired
	ComplaintHandler complaintHandler;

	@Mock
	StreamService streamService;

	@Mock
	MasterDataService masterDataService;

	@Mock
	PersistenceService db;
	@Mock
	Result result;
	@Mock
	Row row;
	@Mock
	CqnInsert insert;
	@Mock
	CdsReadEventContext context;
	@Mock
	ComplaintService complaintService;
	@Mock
	AuditLogHelper auditLogHelper;
	@Mock
	ComplaintValidation validator;
	@Mock
	Messages messages;

	@Mock
	UserInfo info;

	@Mock
	QualityNotificationService qualityNotificationService;
	@Mock
	ClaimService claimService;

	@Mock
	ParameterInfo parameterInfo;

	@Mock
	DestinationService destinationService;

	@Mock
	DestinationConfigurationDao destinationConfigurationDao;

	@Mock
	private Iterator<ScpCfDestination> readAllDestinations;

	@Mock
	private ScpCfDestination getDestination;

	@Mock
	HttpDestination asHttp;

	@Mock
	URI uri;
	@Mock
	CommonFunctions commonFunctions;
	@Mock
	private AuditLogDifference auditLogDifference;
	@Mock
	EightDService eightDService;

	private Complaints complaint;

	private Plants plants;

	private Streams stream;

	private List<Complaints> complaintList = new ArrayList<>();

	private List<Streams> streamList = new ArrayList<>();
	List<Claim> data = new ArrayList<>();

	private CompanyCodes code;

	private Row row1;
	private Optional<Row> opt;
	private static ReturnPurchaseOrder returnPurchaseOrders;
	private Supplier8DProcess supplierEightD;
	private Claim claim;
	private QualityNotification qualityNotifications;
	private final List<Supplier8DProcess> eightDList = new ArrayList<>();

	@Before
	public void beforeClass() {
		MockitoAnnotations.openMocks(this);
		complaint = Struct.create(Complaints.class);
		complaint.setId("1112");
		complaint.setPlantId("F009");
		complaint.setCompanyCodeId("F009");

		complaint.setComplaintStatusCode("CRTD");
		complaintList.add(complaint);
		stream = Struct.create(Streams.class);

		stream.setIsRelevant(true);
		stream.setStreamTypeCode("QLTY");
		stream.setIsRelevant(true);
		stream.setParentIDId("1112");

		streamList.add(stream);
		plants = Struct.create(Plants.class);
		plants.setCompanyCode("F066");
		code = Struct.create(CompanyCodes.class);
		code.setId("F001");

		row1 = Struct.create(Row.class);

		returnPurchaseOrders = Struct.create(ReturnPurchaseOrder.class);
		returnPurchaseOrders.setId("123");
		returnPurchaseOrders.setCompanyId("201");
		returnPurchaseOrders.setComplaintId("101");
		// returnPurchaseOrders.setContactPerson("sap");
		returnPurchaseOrders.setReturnPurchaseType("PO");
		returnPurchaseOrders.setIdentifier("01234");
		returnPurchaseOrders.setSupplierId("201");
		returnPurchaseOrders.setStatusCode("RPOCRTD");

		supplierEightD = Struct.create(Supplier8DProcess.class);
		supplierEightD.setId("123");
		supplierEightD.setStatusCode("INTL");
		supplierEightD.setComplaintId("12");
		supplierEightD.setIdentifier("01234");
		eightDList.add(supplierEightD);

		claim = Struct.create(Claim.class);
		claim.setId("ClaimID");
		claim.setStatusCode("INTL");
		claim.setComplaintId("1234");
		claim.setMaterialId("F001");
		claim.setPlantId("I001");
		claim.setSupplierId("F001");
		claim.setPurchasingOrganizationId("01");
		claim.setQuantity(new BigDecimal(5000));
		claim.setUnit("KL");
		claim.setCompanyId("o01");
		claim.setPersonResponsibleId("I773");
		claim.setCompanyId("I776");
		data.add(claim);

		qualityNotifications = Struct.create(QualityNotification.class);
		qualityNotifications.setCompanyId("100");
		qualityNotifications.setId(UUID.randomUUID().toString());
		qualityNotifications.setComplaintId("101");
		// qualityNotifications.setContactPerson("syyed");
		qualityNotifications.setMaterialId("F01");
		qualityNotifications.setSupplierId("201");
		qualityNotifications.setPlantId("30");
		qualityNotifications.setStatusCode("QNCRTD");
	}

	@Test
	public void beforeComplaintPatchTest() {
		Complaints complaints = Struct.create(Complaints.class);
		complaints.setPlantId("34");
		complaints.setCompanyCodeId("23");
		Plants plants = Struct.create(Plants.class);
		CompanyCodes companyCodes = Mockito.mock(CompanyCodes.class);
		companyCodes.setId("202");
		when(masterDataService.getCompanyCodeBasedOnPlants(any(String.class))).thenReturn(plants);
		when(masterDataService.getCurrencyBasedOnCompanyCodes(any(String.class))).thenReturn(companyCodes);
		when(masterDataService.getCompanyCodeID(companyCodes.getId())).thenReturn(companyCodes);
		complaintHandler.beforeComplaintPatch(complaint);
	}
	@Test
	public void beforeComplaintPatch2Test() {
		Complaints complaints = Struct.create(Complaints.class);
		complaints.setPlantId("34");
		complaints.setCompanyCodeId("23");
		Plants plants = Struct.create(Plants.class);
		CompanyCodes companyCodes = Mockito.mock(CompanyCodes.class);
		companyCodes.setId("202");
		complaints.setLaborUnitCode("1232");

		when(masterDataService.getCompanyCodeBasedOnPlants(any(String.class))).thenReturn(plants);
		when(masterDataService.getCurrencyBasedOnCompanyCodes(any(String.class))).thenReturn(companyCodes);
		when(masterDataService.getCompanyCodeID(companyCodes.getId())).thenReturn(companyCodes);
		complaintHandler.beforeComplaintPatch(complaints);
	}

	@Test
	public void beforeComplaintDraftCreationTest() {
		Complaints complaints = Struct.create(Complaints.class);
		complaints.setPlantId("34");
		complaints.setCompanyCodeId("23");
		complaint.setIsFieldControlMandatory(Constants.FIELD_CONTROL_MANDATORY);
		complaint.setTotalLaborHour(new BigDecimal(0));
		complaint.setTotalSubLetCost(new BigDecimal(0));
		complaintHandler.beforeComplaintDraftCreation(complaint);
	}

	@Test
	public void beforeComplaintCreateTest() {
		Complaints complaints = Struct.create(Complaints.class);
		complaints.setPlantId("34");
		complaints.setCompanyCodeId("23");
		Plants plants = Struct.create(Plants.class);
		CompanyCodes companyCodes = Mockito.mock(CompanyCodes.class);
		companyCodes.setId("202");
		when(masterDataService.getCompanyCodeBasedOnPlants(any(String.class))).thenReturn(plants);
		when(masterDataService.getCurrencyBasedOnCompanyCodes(any(String.class))).thenReturn(companyCodes);
		when(masterDataService.getCompanyCodeID(companyCodes.getId())).thenReturn(companyCodes);
		complaintHandler.beforeComplaintCreate(complaint);
	}

	@Test
	public void testBeforeComplaintCreate() {
		Plants plant = Struct.create(Plants.class);
		plant.setId("FP01");
		CompanyCodes codes = Struct.create(CompanyCodes.class);
		code.setCurrencyCode("F009");
		complaint.setCreationType("DFG");
		when(masterDataService.getCompanyCodeBasedOnPlants(any(String.class))).thenReturn(plant);

		when(masterDataService.getCompanyCodeID(plant.getCompanyCode())).thenReturn(codes);

		when(masterDataService.getCurrencyBasedOnCompanyCodes(complaint.getCompanyCodeId())).thenReturn(codes);

		complaintHandler.beforeComplaintCreate(complaint);
	}

	@Test
	public void testBeforeComplaintCreateCreationTypeValue() {
		Plants plant = Struct.create(Plants.class);
		plant.setId("FP01");
		CompanyCodes codes = Struct.create(CompanyCodes.class);
		code.setCurrencyCode("F009");
		complaint.setCreationType("Automatic");
		when(masterDataService.getCompanyCodeBasedOnPlants(any(String.class))).thenReturn(plant);
		when(masterDataService.getCompanyCodeID(plant.getCompanyCode())).thenReturn(codes);
		when(masterDataService.getCurrencyBasedOnCompanyCodes(complaint.getCompanyCodeId())).thenReturn(codes);
		complaintHandler.beforeComplaintCreate(complaint);
	}

	@Test
	public void onComplaintCreateTest() {
		Complaints complaints = Struct.create(Complaints.class);
		complaints.setPlantId("34");
		complaints.setCompanyCodeId("23");
		Plants plants = Struct.create(Plants.class);
		CompanyCodes companyCodes = Mockito.mock(CompanyCodes.class);
		companyCodes.setId("202");
		when(complaintService.getAllComplaints()).thenReturn(result);
		complaintHandler.onComplaintCreate(complaint);
	}

	@Test
	public void testOnComplaintCreateTestResultNotNull() {
		Complaints complaints = Struct.create(Complaints.class);
		complaints.setPlantId("34");
		complaints.setCompanyCodeId("23");
		complaint.setCreationType(null);
		Plants plants = Struct.create(Plants.class);
		CompanyCodes companyCodes = Mockito.mock(CompanyCodes.class);
		companyCodes.setId("202");
		when(complaintService.getAllComplaints()).thenReturn(result);
		List<Row> rowvalues = new ArrayList<>();
		row1.put("isRelevant", "ComplaintID");
		opt = Optional.of(row1);
		rowvalues.add(row1);
		when(result.list()).thenReturn(rowvalues);
		when(result.first()).thenReturn(opt);
		complaintHandler.onComplaintCreate(complaint);
	}

	@Test
	public void testOnComplaintCreateCreationTypeEmpty() {
		Complaints complaints = Struct.create(Complaints.class);
		complaints.setPlantId("34");
		complaints.setCompanyCodeId("23");
		complaint.setCreationType("");
		Plants plants = Struct.create(Plants.class);
		CompanyCodes companyCodes = Mockito.mock(CompanyCodes.class);
		companyCodes.setId("202");
		when(complaintService.getAllComplaints()).thenReturn(result);
		complaintHandler.onComplaintCreate(complaint);
	}

	@Test
	public void afterComplaintCreateTest() {
		when(complaintService.getComplaintDetails(complaint.getId())).thenReturn(complaint);
		assertDoesNotThrow(
				() -> complaintHandler.afterComplaintCreate(complaint));

	}

	@Test
	public void afterComplaintUpdateTest() {
		Complaints complaints = Struct.create(Complaints.class);
		complaints.setPlantId("34");
		complaints.setCompanyCodeId("23");
		when(complaintService.getComplaintDetails(any(String.class))).thenReturn(complaint);
		when(context.getUserInfo()).thenReturn(info);
		when(info.hasRole(any(String.class))).thenReturn(true);
		when(complaintService.getComplaintDetails(complaints.getId())).thenReturn(complaints);
		complaintHandler.afterComplaintUpdate(complaints);
	}

	@Test
	public void afterComplaintUpdatenull2Test() {
		Complaints complaints = Struct.create(Complaints.class);
		complaints.setPlantId("34");
		complaints.setCompanyCodeId("23");
		when(complaintService.getComplaintDetails(any(String.class))).thenReturn(null);
		when(context.getUserInfo()).thenReturn(info);
		complaints.setComplaintStatusCode("REVSD");
		complaints.setId("2232");
		when(info.hasRole(any(String.class))).thenReturn(true);
		when(complaintService.getComplaintDetails(complaints.getId())).thenReturn(null);
		when(streamService.checkIfAllRelevantStreamsClosed(any(String.class))).thenReturn(true);
		complaintHandler.afterComplaintUpdate(complaints);
	}
	@Test
	public void afterComplaintUpdateINPRTest() {
		Complaints complaints = Struct.create(Complaints.class);
		complaints.setPlantId("34");
		complaints.setCompanyCodeId("23");
		when(complaintService.getComplaintDetails(any(String.class))).thenReturn(null);
		when(context.getUserInfo()).thenReturn(info);
		complaints.setComplaintStatusCode("INPR");
		complaints.setId("2232");
		when(info.hasRole(any(String.class))).thenReturn(true);
		when(complaintService.getComplaintDetails(complaints.getId())).thenReturn(null);
		when(streamService.checkIfAllRelevantStreamsClosed(any(String.class))).thenReturn(true);
		complaintHandler.afterComplaintUpdate(complaints);
	}
	@Test
	public void afterComplaintUpdateRevisedStatusTest() {
		Complaints complaints = Struct.create(Complaints.class);
		complaints.setComplaintStatusCode("REVSD");
		when(complaintService.getComplaintDetails(complaints.getId())).thenReturn(complaints);
		complaintHandler.afterComplaintUpdate(complaints);
	}

	@Test
	public void afterComplaintUpdateRevisedStatusAllStreamsNotClosedTest() {
		Complaints complaints = Struct.create(Complaints.class);
		complaints.setId("1");
		complaints.setComplaintStatusCode("REVSD");
		when(streamService.checkIfAllRelevantStreamsClosed(complaints.getId())).thenReturn(false);
		when(complaintService.getComplaintDetails(complaints.getId())).thenReturn(complaints);
		complaintHandler.afterComplaintUpdate(complaints);
	}

	@Test
	public void testBeforeComplaintReadAddress() {
		doNothing().when(commonFunctions).checkBeforeRead(context);
		complaintHandler.beforeComplaintReadAddress(context);
	}

	@Test
	public void testBeforeComplaintReadBusinessPartner() {
		doNothing().when(commonFunctions).checkBeforeRead(context);
		complaintHandler.beforeComplaintReadBusinessPartner(context);
	}

	@Test
	public void afterComplaintReadTest() {
		BTPUsers users = Struct.create(BTPUsers.class);
		users.setPersonResponsibleNumber("abcd");
		users.setPersonResponsibleId("1234");
		List<BTPUsers> userList = new ArrayList();
		userList.add(users);
		Complaints complaints = Struct.create(Complaints.class);
		complaints.setPlantId("34");
		complaints.setCompanyCodeId("23");
		complaints.setComplaintStatusCode("CRTD");
		complaint.setIsFieldControlMandatory(Constants.FIELD_CONTROL_MANDATORY);
		when(complaintService.getComplaintDetails(any(String.class))).thenReturn(complaint);
		when(context.getUserInfo()).thenReturn(info);
		when(info.hasRole(any(String.class))).thenReturn(true);
		when(complaintService.getAllResponsiblePerson()).thenReturn(userList);
		complaintHandler.afterComplaintRead(context, complaintList);
	}
	@Test
	public void afterComplaintReadDISCDTest() {
		BTPUsers users = Struct.create(BTPUsers.class);
		users.setPersonResponsibleNumber("abcd");
		users.setPersonResponsibleId("1234");
		List<BTPUsers> userList = new ArrayList();
		userList.add(users);
		Complaints complaints = Struct.create(Complaints.class);
		complaints.setPlantId("34");
		complaints.setCompanyCodeId("23");
		complaints.setComplaintStatusCode("DISCD");

		complaint.setIsFieldControlMandatory(Constants.FIELD_CONTROL_MANDATORY);
		when(complaintService.getComplaintDetails(any(String.class))).thenReturn(complaints);
		when(context.getUserInfo()).thenReturn(info);
		when(info.hasRole(any(String.class))).thenReturn(true);
		when(complaintService.getAllResponsiblePerson()).thenReturn(userList);
		complaintHandler.afterComplaintRead(context, complaintList);
	}
	@Test
	public void afterComplaintReadnullTest() {
		BTPUsers users = Struct.create(BTPUsers.class);
		users.setPersonResponsibleNumber("abcd");
		users.setPersonResponsibleId("1234");
		List<BTPUsers> userList = new ArrayList();
		userList.add(users);
		Complaints complaints = Struct.create(Complaints.class);
		complaints.setPlantId("34");
		complaints.setCompanyCodeId("23");
		complaints.setComplaintStatusCode(null);

		complaint.setIsFieldControlMandatory(Constants.FIELD_CONTROL_MANDATORY);
		when(complaintService.getComplaintDetails(any(String.class))).thenReturn(complaints);
		when(context.getUserInfo()).thenReturn(info);
		when(info.hasRole(any(String.class))).thenReturn(true);
		when(complaintService.getAllResponsiblePerson()).thenReturn(userList);
		complaintHandler.afterComplaintRead(context, complaintList);
	}
	@Test
	public void checkCostCollectionButtonHidingNullTest()
	{
		when(qualityNotificationService.getQualityNotificationDetailsByComplaintId(complaint.getId())).thenReturn(Struct.create(QualityNotifications.class));
		when(claimService.getClaim(complaint.getId())).thenReturn(Struct.create(Claims.class));
		complaintHandler.checkCostCollectionButtonHiding(complaint,false);
	}
	@Test
	public void checkCostCollectionButtonHidingTest()
	{
		when(qualityNotificationService.getQualityNotificationDetailsByComplaintId(complaint.getId())).thenReturn(Struct.create(QualityNotifications.class));
		when(claimService.getClaim(complaint.getId())).thenReturn(null);
		complaintHandler.checkCostCollectionButtonHiding(complaint,false);
	}
	@Test
	public void setFlagsWhenComplaintIsClosedTest()
	{
		Complaints complaints=Struct.create(Complaints.class);
		complaints.setIsActiveEntity(true);
		Map<Integer, String> map=new HashMap<>();
		complaintHandler.setFlagsWhenComplaintIsClosed(true,complaints,complaint,map);
	}
	@Test
	public void setFlagsWhenComplaintIsClosedHasActiveTest()
	{
		Complaints complaints=Struct.create(Complaints.class);
		complaints.setIsActiveEntity(true);
		complaints.setHasActiveEntity(false);
		Map<Integer, String> map=new HashMap<>();
		complaintHandler.setFlagsWhenComplaintIsClosed(true,complaints,complaint,map);
	}
	@Test
	public void setFlagsWhenComplaintIsClosedHasActivetrueTest()
	{
		Complaints complaints=Struct.create(Complaints.class);
		complaints.setIsActiveEntity(false);
		complaints.setPersonResponsibleId("12");
		complaints.setHasActiveEntity(true);
		Map<Integer, String> map=new HashMap<>();
		map.put(1,		"12");
		boolean hasActive= Boolean.parseBoolean(null);
		complaintHandler.setFlagsWhenComplaintIsClosed(true,complaints,complaint,map);
	}
	@Test
	public void afterComplaintReadREVSDTest() {
		BTPUsers users = Struct.create(BTPUsers.class);
		users.setPersonResponsibleNumber("abcd");
		users.setPersonResponsibleId("1234");
		List<BTPUsers> userList = new ArrayList();
		userList.add(users);
		Complaints complaints = Struct.create(Complaints.class);
		complaints.setPlantId("34");
		complaints.setCompanyCodeId("23");
		complaints.setComplaintStatusCode("REVSD");
		complaints.setIsActiveEntity(true);
		complaintList.add(complaints);
		complaint.setIsFieldControlMandatory(Constants.FIELD_CONTROL_MANDATORY);
		when(complaintService.getComplaintDetails(any(String.class))).thenReturn(complaints);
		when(context.getUserInfo()).thenReturn(info);
		when(info.hasRole(any(String.class))).thenReturn(true);
		when(complaintService.getAllResponsiblePerson()).thenReturn(userList);
		when(streamService.checkIfAllRelevantStreamsClosed(complaints.getId())).thenReturn(false);
		complaintHandler.afterComplaintRead(context, complaintList);
	}
	@Test
	public void testSetComplaintValues() {
		ComplaintStatuses statuses = Struct.create(ComplaintStatuses.class);
		statuses.setCode("F0038");
		ComplaintCategories types = Struct.create(ComplaintCategories.class);
		types.setCode("F0038");
		when(complaintService.getDefaultStatus()).thenReturn(statuses);
		when(complaintService.getDefaultType()).thenReturn(types);
		complaintHandler.setComplaintValues(complaint);

	}

	@Test
	public void beforeComplaintUpdateTest() {
		Plants plant = Struct.create(Plants.class);
		plant.setId("FP01");
		CompanyCodes codes = Struct.create(CompanyCodes.class);
		code.setId("FP09");
		when(masterDataService.getCompanyCodeBasedOnPlants(any(String.class))).thenReturn(plant);

		when(masterDataService.getCompanyCodeID(plant.getCompanyCode())).thenReturn(codes);

		when(masterDataService.getCurrencyBasedOnCompanyCodes(complaint.getCompanyCodeId())).thenReturn(codes);
		complaintHandler.beforeComplaintUpdate(complaint);

	}

	@Test
	public void testBeforeComplaintUpdate1() {
		complaint.setCreationType("SDG");
		Plants plant = Struct.create(Plants.class);
		plant.setId("FP01");
		CompanyCodes codes = Struct.create(CompanyCodes.class);
		code.setId("FP09");
		when(masterDataService.getCompanyCodeBasedOnPlants(any(String.class))).thenReturn(plant);

		when(masterDataService.getCompanyCodeID(plant.getCompanyCode())).thenReturn(codes);

		when(masterDataService.getCurrencyBasedOnCompanyCodes(complaint.getCompanyCodeId())).thenReturn(codes);
		complaintHandler.beforeComplaintUpdate(complaint);

	}

	@Test
	public void testBeforeComplaintUpdate() {
		complaint.setComplaintStatusCode("ASCRTD");
		Plants plant = Struct.create(Plants.class);
		plant.setId("FP01");
		CompanyCodes codes = Struct.create(CompanyCodes.class);
		code.setId("FP09");
		when(masterDataService.getCompanyCodeBasedOnPlants(any(String.class))).thenReturn(plant);

		when(masterDataService.getCompanyCodeID(plant.getCompanyCode())).thenReturn(codes);

		when(masterDataService.getCurrencyBasedOnCompanyCodes(complaint.getCompanyCodeId())).thenReturn(codes);
		complaintHandler.beforeComplaintUpdate(complaint);

	}

	@Test
	public void beforeComplaintPatchElseTest() {
		Plants plants = Struct.create(Plants.class);
		CompanyCodes companyCodes = Mockito.mock(CompanyCodes.class);
		companyCodes.setId("202");
		companyCodes.setCompanyCode("303");
		complaint.setCompanyCodeId("34");
		plants.setCompanyCodeIDId("2002");
		complaint.setPlant(plants);
		when(masterDataService.getCompanyCodeBasedOnPlants(complaint.getPlantId())).thenReturn(plants);
		when(masterDataService.getCurrencyBasedOnCompanyCodes(any(String.class))).thenReturn(companyCodes);
		when(masterDataService.getCompanyCodeID(companyCodes.getId())).thenReturn(companyCodes);

		complaintHandler.beforeComplaintPatch(complaint);
	}

	@Test
	public void beforeComplaintPatchNullTest() {
		Plants plants = Struct.create(Plants.class);
		CompanyCodes companyCodes = Mockito.mock(CompanyCodes.class);
		companyCodes.setId("202");
		companyCodes.setCompanyCode("303");
		complaint.setCompanyCodeId("34");
		plants.setCompanyCodeIDId(null);
		complaint.setPlantId(null);
		complaint.setPlant(plants);
		when(masterDataService.getCompanyCodeBasedOnPlants(complaint.getPlantId())).thenReturn(plants);
		when(masterDataService.getCurrencyBasedOnCompanyCodes(any(String.class))).thenReturn(companyCodes);
		when(masterDataService.getCompanyCodeID(companyCodes.getId())).thenReturn(companyCodes);

		complaintHandler.beforeComplaintPatch(complaint);
	}

	@Test
	public void afterComplaintUpdateElseTest() {
		Complaints complaints = Struct.create(Complaints.class);
		complaints.setPlantId("34");
		complaints.setId("78");
		complaints.setCompanyCodeId("23");
		complaints.setComplaintStatusCode("CLSD");
		when(complaintService.getComplaintDetails(complaints.getId())).thenReturn(complaints);
		when(context.getUserInfo()).thenReturn(info);
		when(info.hasRole(any(String.class))).thenReturn(true);
		when(complaintService.getComplaintDetails(complaints.getId())).thenReturn(complaints);
		complaintHandler.afterComplaintUpdate(complaints);
	}

	@Test
	public void testAfterComplaintUpdateElse() {
		Complaints complaints = Struct.create(Complaints.class);
		complaints.setPlantId("34");
		complaints.setId("78");
		complaints.setCompanyCodeId("23");
		complaints.setComplaintStatusCode("ACLSA");
		when(complaintService.getComplaintDetails(complaints.getId())).thenReturn(complaints);
		when(context.getUserInfo()).thenReturn(info);
		when(info.hasRole(any(String.class))).thenReturn(true);
		when(complaintService.getComplaintDetails(complaints.getId())).thenReturn(complaints);
		complaintHandler.afterComplaintUpdate(complaints);
	}

	@Test
	public void afterComplaintUpdateNullTest() {
		Complaints complaints = Struct.create(Complaints.class);
		complaints.setPlantId("34");
		complaints.setId("78");
		complaints.setCompanyCodeId("23");
		complaints.setComplaintStatusCode("NEW");
		when(complaintService.getComplaintDetails(complaints.getId())).thenReturn(null);
		when(context.getUserInfo()).thenReturn(info);
		when(info.hasRole(any(String.class))).thenReturn(true);
		when(complaintService.getComplaintDetails(complaints.getId())).thenReturn(complaints);
		complaintHandler.afterComplaintUpdate(complaints);
	}

	@Test
	public void testAfterComplaintUpdateNull() {
		Complaints complaints = Struct.create(Complaints.class);
		complaints.setPlantId("34");
		complaints.setId("78");
		complaints.setCompanyCodeId("23");
		complaints.setComplaintStatusCode(null);
		when(complaintService.getComplaintDetails(complaints.getId())).thenReturn(complaints);
		when(context.getUserInfo()).thenReturn(info);
		when(info.hasRole(any(String.class))).thenReturn(true);
		when(complaintService.getComplaintDetails(complaints.getId())).thenReturn(complaints);
		complaintHandler.afterComplaintUpdate(complaints);
	}

	@Test
	public void afterComplaintReadElseTest() {
		Complaints complaints = Struct.create(Complaints.class);
		complaints.setPlantId("34");
		complaints.setId("1");
		complaints.setCompanyCodeId("23");
		complaints.setComplaintStatusCode("INPR");
		complaints.setIsComplaintNew(1);
		List<Complaints> complaintsList = new ArrayList<>();
		complaintsList.add(complaints);
		when(complaintService.getComplaintDetails(complaints.getId())).thenReturn(complaints);
		when(context.getUserInfo()).thenReturn(info);
		when(info.hasRole(any(String.class))).thenReturn(true);
		complaintHandler.afterComplaintRead(context, complaintsList);
	}

	@Test
	public void testAfterComplaintReadComplaintsNull() {
		Complaints complaints = Struct.create(Complaints.class);
		complaints.setPlantId("34");
		complaints.setId("1");
		complaints.setCompanyCodeId("23");
		complaints.setComplaintStatusCode("CLSD");
		complaints.setIsComplaintNew(1);
		List<Complaints> complaintsList = new ArrayList<>();
		complaintsList.add(complaints);
		when(complaintService.getComplaintDetails(complaints.getId())).thenReturn(null);
		when(context.getUserInfo()).thenReturn(info);
		when(info.hasRole(any(String.class))).thenReturn(false);
		complaintHandler.afterComplaintRead(context, complaintsList);
	}

	@Test
	public void afterComplaintReadElse1Test() {
		Complaints complaints = Struct.create(Complaints.class);
		complaints.setPlantId("34");
		complaints.setId("1");
		complaints.setCompanyCodeId("23");
		complaints.setComplaintStatusCode("CLSD");
		complaints.setIsComplaintNew(1);
		List<Complaints> complaintsList = new ArrayList<>();
		complaintsList.add(complaints);
		when(complaintService.getComplaintDetails(complaints.getId())).thenReturn(complaints);
		when(context.getUserInfo()).thenReturn(info);
		when(info.hasRole(any(String.class))).thenReturn(true);
		complaintHandler.afterComplaintRead(context, complaintsList);
	}

	@Test
	public void testafterComplaintReadBooleanFalse() {
		Complaints complaints = Struct.create(Complaints.class);
		complaints.setPlantId("34");
		complaints.setId("1");
		complaints.setCompanyCodeId("23");
		complaints.setComplaintStatusCode("CLSD");
		complaints.setIsComplaintNew(1);
		complaints.setIsActiveEntity(true);
		complaints.setHasActiveEntity(true);
		List<Complaints> complaintsList = new ArrayList<>();
		complaintsList.add(complaints);
		when(complaintService.getComplaintDetails(complaints.getId())).thenReturn(complaints);
		when(context.getUserInfo()).thenReturn(info);
		when(info.hasRole(any(String.class))).thenReturn(false);
		complaintHandler.afterComplaintRead(context, complaintsList);
	}

	@Test
	public void testafterComplaintReadHasCostCollectorTrue() {
		Claims claim = Struct.create(Claims.class);
		claim.setId("12");
		Complaints complaints = Struct.create(Complaints.class);
		complaints.setPlantId("34");
		complaints.setId("1");
		complaints.setCompanyCodeId("23");
		complaints.setComplaintStatusCode("CLSD");
		complaints.setIsComplaintNew(1);
		complaints.setIsActiveEntity(true);
		complaints.setHasActiveEntity(true);
		List<Complaints> complaintsList = new ArrayList<>();
		complaintsList.add(complaints);
		when(complaintService.getComplaintDetails(complaints.getId())).thenReturn(complaints);
		when(context.getUserInfo()).thenReturn(info);
		when(info.hasRole(any(String.class))).thenReturn(true);
		when(claimService.getClaim(any())).thenReturn(claim);
		complaintHandler.afterComplaintRead(context, complaintsList);
	}

	@Test
	public void testafterComplaintReadHasCostCollectorQnNotNull() {

		BTPUsers btpUsers = Struct.create(BTPUsers.class);
		btpUsers.setPersonResponsibleId("abc");
		btpUsers.setPersonResponsibleNumber("123456789");
		List<BTPUsers> list = new ArrayList<>();
		list.add(btpUsers);
		QualityNotifications qn = Struct.create(QualityNotifications.class);
		qn.setQnType("SDR");
		Claims claim = Struct.create(Claims.class);
		claim.setId("12");
		Complaints complaints = Struct.create(Complaints.class);
		complaints.setPersonResponsibleId("12");
		complaints.setPlantId("34");
		complaints.setId("1");
		complaints.setCompanyCodeId("23");
		complaints.setComplaintStatusCode("CLSD");
		complaints.setIsComplaintNew(1);
		complaints.setIsActiveEntity(true);
		complaints.setHasActiveEntity(true);
		List<Complaints> complaintsList = new ArrayList<>();
		complaintsList.add(complaints);
		when(complaintService.getComplaintDetails(complaints.getId())).thenReturn(complaints);
		when(context.getUserInfo()).thenReturn(info);
		when(info.hasRole(any(String.class))).thenReturn(true);
		when(qualityNotificationService.getQualityNotificationDetailsByComplaintId(any())).thenReturn(qn);
		when(claimService.getClaim(any())).thenReturn(null);
		when(complaintService.getAllResponsiblePerson()).thenReturn(list);
		complaintHandler.afterComplaintRead(context, complaintsList);
	}

	@Test
	public void afterComplaintReadNullTest() {
		Complaints complaints = Struct.create(Complaints.class);
		complaints.setPlantId("34");
		complaints.setId("1");
		complaints.setCompanyCodeId("23");
		complaints.setComplaintStatusCode("CLSD");
		complaints.setIsComplaintNew(1);
		List<Complaints> complaintsList = new ArrayList<>();
		complaintsList.add(complaints);
		when(complaintService.getComplaintDetails(complaints.getId())).thenReturn(null);
		when(context.getUserInfo()).thenReturn(info);
		when(info.hasRole(any(String.class))).thenReturn(true);
		complaintHandler.afterComplaintRead(context, complaintsList);
	}

	@Test
	public void afterComplaintReadRevisedStatusStreamsOpenTest() {
		Complaints complaints = Struct.create(Complaints.class);
		complaints.setComplaintStatusCode("REVSD");
		List<Complaints> complaintsList = new ArrayList<>();
		complaintsList.add(complaints);
		when(complaintService.getComplaintDetails(complaints.getId())).thenReturn(complaints);
		when(context.getUserInfo()).thenReturn(info);
		when(info.hasRole(any(String.class))).thenReturn(true);
		complaintHandler.afterComplaintRead(context, complaintsList);
	}

	@Test
	public void afterComplaintReadRevisedStatusStreamsClosedTest() {
		Complaints complaints = Struct.create(Complaints.class);
		complaints.setComplaintStatusCode("REVSD");
		complaints.setIsActiveEntity(true);
		List<Complaints> complaintsList = new ArrayList<>();
		complaintsList.add(complaints);
		when(complaintService.getComplaintDetails(complaints.getId())).thenReturn(complaints);
		when(context.getUserInfo()).thenReturn(info);
		when(info.hasRole(any(String.class))).thenReturn(true);
		when(streamService.checkIfAllRelevantStreamsClosed(complaints.getId())).thenReturn(true);
		complaintHandler.afterComplaintRead(context, complaintsList);
	}

	@Test
	public void testAfterBTPUsersRead() {
		Map<String, String> map1 = new HashMap<>();
		map1.put("$filter", "we are gg");
		map1.put("$search", " ");
		map1.put("$select", "plantCode");
		when(parameterInfo.getQueryParams()).thenReturn(map1);
		when(context.getParameterInfo()).thenReturn(parameterInfo);
		complaintHandler.beforeBTPUsersRead(context);
	}

	@Test
	public void testAfterBTPUsersRead1() {
		BTPUsers btpUsers = Struct.create(BTPUsers.class);
		btpUsers.setPersonResponsibleId("12");
		btpUsers.setPersonResponsibleNumber("I540419");
		List<BTPUsers> list = new ArrayList<>();
		list.add(btpUsers);
		Map<String, String> map1 = new HashMap<>();
		map1.put("$filter", "personResponsibleNumber eq 'I540'");
		map1.put("$search", null);
		when(parameterInfo.getQueryParams()).thenReturn(map1);
		when(context.getParameterInfo()).thenReturn(parameterInfo);
		when(complaintService.getAllResponsiblePerson()).thenReturn(list);
		complaintHandler.beforeBTPUsersRead(context);
	}
	
	@Test
	public void testAfterBTPUsersReadFilterId() {
		BTPUsers btpUsers = Struct.create(BTPUsers.class);
		btpUsers.setPersonResponsibleId("shruthi.anantharaman@sap.com");
		btpUsers.setPersonResponsibleNumber("I540419");
		List<BTPUsers> list = new ArrayList<>();
		list.add(btpUsers);
		Map<String, String> map1 = new HashMap<>();
		map1.put("$filter", "personResponsibleId eq 'shrut'");
		map1.put("$search", null);
		when(parameterInfo.getQueryParams()).thenReturn(map1);
		when(context.getParameterInfo()).thenReturn(parameterInfo);
		when(complaintService.getAllResponsiblePerson()).thenReturn(list);
		complaintHandler.beforeBTPUsersRead(context);
	}

	@Test
	public void testAfterBTPUsersReadIfElseCondition() {
		BTPUsers btpUsers = Struct.create(BTPUsers.class);
		btpUsers.setPersonResponsibleId("shruthi.anantharaman@sap.com");
		btpUsers.setPersonResponsibleNumber("5678885444");
		List<BTPUsers> list = new ArrayList<>();
		list.add(btpUsers);
		Map<String, String> map1 = new HashMap<>();
		map1.put("$filter", null);
		map1.put("$search", " \"shRU\" ");
		map1.put("$select", "plantCode");
		when(parameterInfo.getQueryParams()).thenReturn(map1);
		when(context.getParameterInfo()).thenReturn(parameterInfo);
		when(complaintService.getAllResponsiblePerson()).thenReturn(list);
		complaintHandler.beforeBTPUsersRead(context);
	}

	@Test
	public void testAfterBTPUsersReadIfElseValidate() {
		BTPUsers btpUsers = Struct.create(BTPUsers.class);
		btpUsers.setPersonResponsibleId("abc");
		btpUsers.setPersonResponsibleNumber("123456789");
		List<BTPUsers> list = new ArrayList<>();
		list.add(btpUsers);
		Map<String, String> map1 = new HashMap<>();
		map1.put("$filter", null);
		map1.put("$search"," \"123456789\" ");
		map1.put("$select", "plantCode");
		when(parameterInfo.getQueryParams()).thenReturn(map1);
		when(context.getParameterInfo()).thenReturn(parameterInfo);
		when(complaintService.getAllResponsiblePerson()).thenReturn(list);
		complaintHandler.beforeBTPUsersRead(context);
	}

	@Test
	public void testAfterBTPUsersReadElseCondition() {
		BTPUsers btpUsers = Struct.create(BTPUsers.class);
		btpUsers.setPersonResponsibleId("shruthi.anantharaman@sap.com");
		btpUsers.setPersonResponsibleNumber("I540419");
		List<BTPUsers> list = new ArrayList<>();
		list.add(btpUsers);
		Map<String, String> map1 = new HashMap<>();
		map1.put("$filter", null);
		map1.put("$search", null);
		map1.put("$select", null);
		when(parameterInfo.getQueryParams()).thenReturn(map1);
		when(context.getParameterInfo()).thenReturn(parameterInfo);
		when(complaintService.getAllResponsiblePerson()).thenReturn(list);
		complaintHandler.beforeBTPUsersRead(context);
	}

	@Test
	public void testhideStream() {
		complaintHandler.hideStream(false, false, false);
	}
	@Test
	public void testhideStreamtrue() {
		complaintHandler.hideStream(false, true, true);
	}
	@Test
	public void testhideStreamtrue2() {
		complaintHandler.hideStream(true, false, true);
	}
	@Test
	public void testhideStreamNull() {

		complaintHandler.hideStream(false, false, null);
	}

	@Test
	public void testafterProcessFlowRead() {
		ProcessFlow pf = Struct.create(ProcessFlow.class);
		pf.setSourceIdentifier("00129123");
		pf.setTargetIdentifier("00251425");
		List<ProcessFlow> processFlowList = new ArrayList<>();
		processFlowList.add(pf);
		complaintHandler.afterProcessFlowRead(context, processFlowList);
	}

	@Test
	public void testafterCommonBusinessObjectsRead() {
		CommonBusinessObjects cbo = Struct.create(CommonBusinessObjects.class);
		cbo.setIdentifier("0025485");
		List<CommonBusinessObjects> commonBOList = new ArrayList<>();
		commonBOList.add(cbo);
		complaintHandler.afterCommonBusinessObjectsRead(context, commonBOList);
	}

	@Test
	public void testLogUpsert() {

		Complaints mockedObj = Complaints.create();
		mockedObj.setId("test-id");
		when(complaintService.getComplaintDetails(anyString())).thenReturn(mockedObj);
		assertDoesNotThrow(() -> complaintHandler.logUpsert(Action.UPDATE,mockedObj));
	}

	@Test
	public void testSetOldAuditData() {
		Complaints complaints = Complaints.create();
		complaints.setId("123");

		Complaints mockAction = mock(Complaints.class);
		when(complaintService.getComplaintDetails(anyString())).thenReturn(mockAction);

		assertDoesNotThrow(
				() -> complaintHandler.setOldAuditData(complaints));
	}

	@Test
	public void testAfterReturnPurchaseOrderReadUpdate() {
		Optional<ReturnPurchaseOrder> emptyOpt = Optional.of(returnPurchaseOrders);
		List<ReturnPurchaseOrder> listPo = new ArrayList<>();
		listPo.add(returnPurchaseOrders);
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		List<Row> rowvalues = new ArrayList<>();
		row.put("navigationDestination", "ComplaintID");
		opt = Optional.of(row);
		rowvalues.add(row);
		when(result.list()).thenReturn(rowvalues);
		when(result.first()).thenReturn(opt);
		when(destinationService.readAllDestination(any(
				ScpCfDestinationLoader.class))).thenReturn(readAllDestinations);
		when(destinationConfigurationDao.getDestinationConfigBasedOnCompanyAndBOType("201",
				Constants.RETURNPO_CODE)).thenReturn(result);
		when(readAllDestinations.hasNext()).thenReturn(true).thenReturn(false);
		when(readAllDestinations.next()).thenReturn(getDestination);
		when(getDestination.getName()).thenReturn("ComplaintID");
		when(getDestination.isHttp()).thenReturn(true);
		complaintHandler.afterReturnPurchaseOrderRead(context, listPo);
	}

	@Test
	public void testAfterSupplier8DRead() {
		Optional<Supplier8DProcess> optional = Optional.of(supplierEightD);
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.first(Supplier8DProcess.class)).thenReturn(optional);
		complaintHandler.afterSupplier8DRead(context, eightDList);
	}

	@Test
	public void testafterClaimRead() {

		when(db.run(any(CqnSelect.class))).thenReturn(result);
		List<Row> rowvalues = new ArrayList<>();
		row.put("navigationDestination", "ComplaintID");
		opt = Optional.of(row);
		rowvalues.add(row);
		when(result.list()).thenReturn(rowvalues);
		when(result.first()).thenReturn(opt);
		when(destinationService.readAllDestination(any(
				ScpCfDestinationLoader.class))).thenReturn(readAllDestinations);
		when(destinationConfigurationDao.getDestinationConfigBasedOnCompanyAndBOType("I776", Constants.CLAIM_CODE))
				.thenReturn(result);
		when(readAllDestinations.hasNext()).thenReturn(true).thenReturn(false);
		when(readAllDestinations.next()).thenReturn(getDestination);
		when(getDestination.getName()).thenReturn("ComplaintID");
		when(getDestination.isHttp()).thenReturn(true);

		complaintHandler.afterClaimRead(context, data);

	}

	@Test
	public void afterQualityNotificationReadTest() {

		List<QualityNotification> emptyOpt = new ArrayList<>();
		emptyOpt.add(qualityNotifications);
		when(destinationService.readAllDestination(any(
				ScpCfDestinationLoader.class))).thenReturn(readAllDestinations);
		// when(complaintService.getComplaintDetails(any(String.class))).thenReturn(complaints);
		when(destinationConfigurationDao.getDestinationConfigBasedOnCompanyAndBOType(
				qualityNotifications.getCompanyId(),
				Constants.QUALITYNOTIFICATION_CODE)).thenReturn(result);
		List<Row> rowvalues = new ArrayList<>();
		row.put("navigationDestination", "ComplaintID");
		opt = Optional.of(row);
		rowvalues.add(row);
		when(result.list()).thenReturn(rowvalues);
		when(result.first()).thenReturn(opt);
		when(readAllDestinations.hasNext()).thenReturn(true).thenReturn(false);
		when(readAllDestinations.next()).thenReturn(getDestination);
		when(getDestination.getName()).thenReturn("ComplaintID");
		when(getDestination.isHttp()).thenReturn(true);
		complaintHandler.afterQualityNotificationRead(context, emptyOpt);
	}
}