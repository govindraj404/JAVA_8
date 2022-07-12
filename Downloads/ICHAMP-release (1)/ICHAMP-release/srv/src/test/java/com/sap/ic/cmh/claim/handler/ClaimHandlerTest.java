package com.sap.ic.cmh.claim.handler;

import cds.gen.claimservice.Claims;
import cds.gen.complaintservice.Complaints;
import cds.gen.costcollectorservice.CostCollectors;
import com.sap.cds.Result;
import com.sap.cds.Row;
import com.sap.cds.Struct;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.cds.CdsReadEventContext;
import com.sap.cds.services.draft.DraftNewEventContext;
import com.sap.cds.services.messages.Messages;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.cds.services.request.UserInfo;
import com.sap.cloud.sdk.cloudplatform.connectivity.HttpDestination;
import com.sap.cloud.sdk.cloudplatform.connectivity.ScpCfDestination;
import com.sap.cloud.sdk.cloudplatform.connectivity.ScpCfDestinationLoader;
import com.sap.ic.cmh.auditlog.AuditLogDifference;
import com.sap.ic.cmh.auditlog.AuditLogHelper;
import com.sap.ic.cmh.businessobjects.service.BusinessObjectService;
import com.sap.ic.cmh.claim.service.ClaimService;
import com.sap.ic.cmh.claim.validations.ClaimValidationImpl;
import com.sap.ic.cmh.complaint.service.ComplaintService;
import com.sap.ic.cmh.complaint.service.StreamService;
import com.sap.ic.cmh.configuration.persistency.DestinationConfigurationDao;
import com.sap.ic.cmh.costcollector.service.CostCollectorService;
import com.sap.ic.cmh.network.service.DestinationService;
import com.sap.ic.cmh.utils.CommonFunctions;
import com.sap.ic.cmh.utils.Constants;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;

import java.math.BigDecimal;
import java.net.URI;
import java.rmi.server.ExportException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Optional;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;
import static org.mockito.Mockito.doNothing;


public class ClaimHandlerTest {

	@InjectMocks
	@Autowired
	ClaimHandler claimCMHHandler;

    @Mock
	DraftNewEventContext context;

    @Mock
	ComplaintService complaintService;

	@Mock
	ClaimService claimService;

	@Mock
	CostCollectorService costCollectorService;

	@Mock
	ClaimValidationImpl claimValidator;

	@Mock
	Messages messages;

	@Mock
	BusinessObjectService businessObjectService;

	@Mock
	StreamService streamService;

	@Mock
	private AuditLogHelper auditLogHelper;

	@Mock
	CdsReadEventContext readContext;

	@Mock
	UserInfo userInfo;

	@Mock
	protected PersistenceService mockDb;

	@Mock
	Result result;

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
UserInfo user;

	@Mock
	CommonFunctions commonFunctions;

    @Mock 
    private AuditLogDifference auditLogDifference;

    

	private Claims claim;

	private Claims claims;

	private Complaints complaint;

	private CostCollectors costCollector;

	List<CostCollectors> cost = new ArrayList<>();

	List<Claims> data = new ArrayList<>();

	private Row row;
	private Optional<Row> opt;


	@Before
	public void beforeClass() {
		MockitoAnnotations.openMocks(this);
		claim = Struct.create(Claims.class);
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

		claims = Struct.create(Claims.class);
		claims.setId("ClaimID");
		claims.setStatusCode("CLMCLSD");
		claims.setComplaintId("1234");
		claims.setMaterialId("F001");
		claims.setPlantId("I001");
		claims.setSupplierId("F001");
		claims.setPurchasingOrganizationId("01");
		claims.setQuantity(new BigDecimal(5000));
		claims.setUnit("KL");
		claims.setCompanyId("o01");
		claims.setPersonResponsibleId("I773");
		claims.setCompanyId("I776");

		row = Struct.create(Row.class);
		complaint = Struct.create(Complaints.class);
		complaint.setId("complaintId");
		complaint.setMaterialId("M01");
		complaint.setCompanyCode(claim);
		complaint.setPlantId("I001");
		complaint.setSupplierId("F001");
		complaint.setPurchasingOrganizationId("01");
		complaint.setQuantity(new BigDecimal(5000));
		complaint.setUnitCode("KL");
		complaint.setCompanyCodeId("o01");
		complaint.setPersonResponsibleId("I773");

		costCollector = Struct.create(CostCollectors.class);
		costCollector.setId("123");
		costCollector.setSubItemTypeCode("001");
		costCollector.setItemTypeCode("1");
		costCollector.setQuantity(BigDecimal.valueOf(100));
		costCollector.setDescription("abcd");
		costCollector.setUnitCode("1");
		costCollector.setTotalCost(BigDecimal.valueOf(1000));
		costCollector.setCurrencyCode("EUR");
		cost.add(costCollector);
	}

	@Test
	public void testBeforeClaimDraftNew() {
		claimCMHHandler.beforeClaimDraftNew(context, claim);
	}


	@Test
	public void testBeforeClaimDraftPatch() {
		when(complaintService.getComplaintDetails(claim.getComplaintId())).thenReturn(complaint);
		when(costCollectorService.selectTransferToClaimCostCollector(claim.getComplaintId())).thenReturn(cost);
		claimCMHHandler.beforeClaimDraftPatch(claim);
	}

	@Test
	public void testBeforeClaimDraftPatchComplaintTypeCodeNull() {
		complaint.setComplaintTypeCode(null);
		when(complaintService.getComplaintDetails(claim.getComplaintId())).thenReturn(complaint);
		when(costCollectorService.selectTransferToClaimCostCollector(claim.getComplaintId())).thenReturn(cost);
		claimCMHHandler.beforeClaimDraftPatch(claim);
	}
	@Test
	public void testBeforeClaimDraftPatchComplaintIdNull() {
		claim.setComplaintId(null);
		when(complaintService.getComplaintDetails(claim.getComplaintId())).thenReturn(complaint);
		when(costCollectorService.selectTransferToClaimCostCollector(claim.getComplaintId())).thenReturn(cost);
		claimCMHHandler.beforeClaimDraftPatch(claim);
	}

	@Test
	public void testBeforeClaimDraftPatchCostCollectorsListNull() {
		when(complaintService.getComplaintDetails(claim.getComplaintId())).thenReturn(complaint);
		when(costCollectorService.selectTransferToClaimCostCollector(claim.getComplaintId())).thenReturn(null);
		claimCMHHandler.beforeClaimDraftPatch(claim);
	}

	@Test
	public void testBeforeCreateClaim() {
		when(complaintService.getComplaintDetails(claim.getComplaintId())).thenReturn(complaint);
		claimCMHHandler.beforeCreateClaim(claim);
	}

	@Test
	public void testOnCreateClaim() {
		when(claimService.createClaimAtDestination(claim)).thenReturn("CLM909");
		when(businessObjectService.getCurrentBOStatus(claim.getId())).thenReturn("INPR");
		claimCMHHandler.onCreateClaim(claim);
		verify(complaintService).updateComplaintStatus(claim.getComplaintId(),
				Constants.COMPLAINT_IN_PROGRESS);

	}

	@Test
	public void testBeforeClaimReadAddress() {
		doNothing().when(commonFunctions).checkBeforeRead(readContext);
		claimCMHHandler.beforeClaimReadAddress(readContext);
	}
	@Test
	public void testBeforeClaimReadBusinessPartner() {
		doNothing().when(commonFunctions).checkBeforeRead(readContext);
		claimCMHHandler.beforeClaimReadBusinessPartner(readContext);
	}



	@Test
	public void testBeforeUpdateClaim() {
		claimCMHHandler.beforeUpdateClaim(claim);
	}

	@Test
	public void testUpdateButtonVisibility() {
		UserInfo user = UserInfo.create();
		user.hasRole("Claim.Update");
		when(readContext.getUserInfo()).thenReturn(user);
		when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
		List<Row> rowvalues = new ArrayList<>();
		row.put("navigationDestination", "ComplaintID");
		opt = Optional.of(row);
		rowvalues.add(row);
		when(result.list()).thenReturn(rowvalues);
		when(result.first()).thenReturn(opt);
		when(destinationService.readAllDestination(any(
				ScpCfDestinationLoader.class))).thenReturn(readAllDestinations);
		when(destinationConfigurationDao.getDestinationConfigBasedOnCompanyAndBOType("I776", Constants.CLAIM_CODE)).thenReturn(result);
		when(claimService.getClaimBasedOnId(claim.getId())).thenReturn(claim);
		when(readAllDestinations.hasNext()).thenReturn(true).thenReturn(false);
		when(readAllDestinations.next()).thenReturn(getDestination);
		when(getDestination.getName()).thenReturn("ComplaintID");
		when(getDestination.isHttp()).thenReturn(true);

		claimCMHHandler.updateButtonVisibility(readContext ,data);

	}

	@Test
	public void testUpdateButtonVisibilityClaimIdNull() {
		claim.setCompanyId(null);
		data.add(claim);
		UserInfo user = UserInfo.create();
		user.hasRole("Claim.Update");
		when(readContext.getUserInfo()).thenReturn(user);
		when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
		List<Row> rowvalues = new ArrayList<>();
		row.put("navigationDestination", "ComplaintID");
		opt = Optional.of(row);
		rowvalues.add(row);
		when(result.list()).thenReturn(rowvalues);
		when(result.first()).thenReturn(opt);
		when(destinationService.readAllDestination(any(
				ScpCfDestinationLoader.class))).thenReturn(readAllDestinations);
		when(destinationConfigurationDao.getDestinationConfigBasedOnCompanyAndBOType("I776", Constants.CLAIM_CODE)).thenReturn(result);
		when(claimService.getClaimBasedOnId(claim.getId())).thenReturn(claims);
		when(readAllDestinations.hasNext()).thenReturn(true).thenReturn(false);
		when(readAllDestinations.next()).thenReturn(getDestination);
		when(getDestination.getName()).thenReturn("ComplaintID");
		when(getDestination.isHttp()).thenReturn(true);
		when(getDestination.asHttp()).thenReturn(asHttp);
		when(asHttp.getUri()).thenReturn(uri);

		claimCMHHandler.updateButtonVisibility(readContext ,data);

	}

	// @Test
	// public void testLogUpsert() {
	// 	claimCMHHandler.logUpsert(data);
	// }


	@Test
	public void testUpdateButtonVisibilityElse() {
		claim.setStatusCode("NEW");
		claim.setIsUpdateRestricted(false);
		UserInfo user = UserInfo.create();
		user.hasRole("Claim.Update");
		when(readContext.getUserInfo()).thenReturn(user);
		when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
		List<Row> rowvalues = new ArrayList<>();
		row.put("navigationDestination", "ComplaintID");
		opt = Optional.of(row);
		rowvalues.add(row);
		when(result.list()).thenReturn(rowvalues);
		when(result.first()).thenReturn(opt);
		when(destinationService.readAllDestination(any(
				ScpCfDestinationLoader.class))).thenReturn(readAllDestinations);
		when(destinationConfigurationDao.getDestinationConfigBasedOnCompanyAndBOType("I776", Constants.CLAIM_CODE)).thenReturn(result);
		when(claimService.getClaimBasedOnId(claim.getId())).thenReturn(claim);
		when(readAllDestinations.hasNext()).thenReturn(true).thenReturn(false);
		when(readAllDestinations.next()).thenReturn(getDestination);
		when(getDestination.getName()).thenReturn("ComplaintID");
		when(getDestination.isHttp()).thenReturn(true);

		claimCMHHandler.updateButtonVisibility(readContext ,data);

	}

	@Test
	public void testBeforeClaimDraftPatchComplaintTypeCodeElse() {
		complaint.setComplaintTypeCode("test");
		when(complaintService.getComplaintDetails(claim.getComplaintId())).thenReturn(complaint);
		when(costCollectorService.selectTransferToClaimCostCollector(claim.getComplaintId())).thenReturn(cost);
		claimCMHHandler.beforeClaimDraftPatch(claim);
	}
 @Test
	public void setMasterDataFromComplaintsTest() {
		when(complaintService.getComplaintDetails(claim.getComplaintId())).thenReturn(null);
		claimCMHHandler.setMasterDataFromComplaints(claim);
	}

	@Test(expected = Exception.class)
	public void testUpdateButtonVisibilityElse1() {
		data=new ArrayList<>();
		Claims claims=Struct.create(Claims.class);
		claims.setStatusCode("CLMCLSD");
		//claims.setIsUpdateRestricted(false);
		data.add(claims);

		when(user.hasRole(any())).thenReturn(true);
		when(readContext.getUserInfo()).thenReturn(user);
		when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
		List<Row> rowvalues = new ArrayList<>();
		row.put("navigationDestination", "ComplaintID");
		opt = Optional.of(row);
		rowvalues.add(row);
		when(result.list()).thenReturn(rowvalues);
		when(result.first()).thenReturn(opt);
		when(destinationService.readAllDestination(any(
				ScpCfDestinationLoader.class))).thenReturn(readAllDestinations);
		when(destinationConfigurationDao.getDestinationConfigBasedOnCompanyAndBOType("I776", Constants.CLAIM_CODE)).thenReturn(result);
		when(claimService.getClaimBasedOnId(any())).thenReturn(claims);
		when(readAllDestinations.hasNext()).thenReturn(true).thenReturn(false);
		when(readAllDestinations.next()).thenReturn(getDestination);
		when(getDestination.getName()).thenReturn("ComplaintID");
		when(getDestination.isHttp()).thenReturn(true);

		claimCMHHandler.updateButtonVisibility(readContext ,data);

	}

	@Test
	public void testBeforeClaimDraftPatchNull() {
		complaint.setComplaintTypeCode("test");
		when(complaintService.getComplaintDetails(claim.getComplaintId())).thenReturn(null);
		when(costCollectorService.selectTransferToClaimCostCollector(claim.getComplaintId())).thenReturn(cost);
		claimCMHHandler.beforeClaimDraftPatch(claim);
	}


    @Test
    public void testLogUpsert() {
        when(claimService.getClaimBasedOnId(anyString())).thenReturn(Claims.create());
        Claims mockedObj = Claims.create();
        mockedObj.setId("test-id");
        assertDoesNotThrow(() -> claimCMHHandler.logUpsert(mockedObj));
    }

    @Test
    public void testSetOldAuditData() {
        Claims claims = Claims.create();
        claims.setId("123");

        Claims mockAction = mock(Claims.class);
        when(claimService.getClaimBasedOnId(anyString())).thenReturn(mockAction);

        assertDoesNotThrow(
                () -> claimCMHHandler.setOldAuditData(claims));
    }

    @Test
    public void testAfterCreateUpdateClaim(){
        when(claimService.getClaimBasedOnId(claim.getId())).thenReturn(claim);
         assertDoesNotThrow(
                () -> claimCMHHandler.afterCreateUpdateClaim(claim));
    }
}


