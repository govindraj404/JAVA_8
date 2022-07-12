package com.sap.ic.cmh.manageclaim.handler;

import cds.gen.manageclaimservice.Claims;
import cds.gen.masterdataservice.BusinessPartners;
import com.sap.cds.Result;
import com.sap.cds.Row;
import com.sap.cds.Struct;
import com.sap.cds.services.ServiceException;
import com.sap.ic.cmh.claim.handler.ClaimHandler;
import com.sap.ic.cmh.claim.service.ClaimService;
import com.sap.ic.cmh.auditlog.AuditLogDifference;
import com.sap.ic.cmh.auditlog.AuditLogHelper;
import com.sap.ic.cmh.complaint.persistency.ComplaintsDao;
import com.sap.ic.cmh.configuration.service.ConfigurationService;
import com.sap.ic.cmh.configuration.service.MessageService;
import com.sap.ic.cmh.utils.CommonFunctions;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import static org.mockito.ArgumentMatchers.anyMap;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.when;

public class ManageClaimHandlerTest {

    @InjectMocks
    @Autowired
    ManageClaimHandler handler;

    @Mock
    ClaimHandler claimHandler;

    @Mock
    CommonFunctions commonFunctions;

    @Mock
    ConfigurationService configurationService;

    @Mock
    ComplaintsDao complaintDao;

    @Mock
    AuditLogHelper auditLogHelper;

    @Mock
    MessageService messageService;




    @Mock
    Result result;
    
    @Mock
    ClaimService claimService;

    @Mock 
    private AuditLogDifference auditLogDifference;

    private Claims manageClaims;
    private cds.gen.claimservice.Claims claims;
    private BusinessPartners partner;
    private Row row;
    private Optional<Row> opt;

    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);

        manageClaims = Struct.create(Claims.class);
        manageClaims.setComplaintCode("F8766");
        manageClaims.setClaimType("F977");
        manageClaims.setComplaintId("F4556");
        manageClaims.setId("123");
        manageClaims.setIdentifier("123");
        manageClaims.setStatusCode("act");

        claims = Struct.create(cds.gen.claimservice.Claims.class);
        claims.setId("123");
        claims.setClaimType("F977");
        claims.setComplaintId("F4556");

        partner = Struct.create(BusinessPartners.class);
        partner.setId("1234");

        row = Struct.create(Row.class);
    }

    @Test
    public void testOnCreateClaim() {
        when(commonFunctions.convertManageClaimsToClaims(manageClaims)).thenReturn(claims);
        handler.onCreateClaim(manageClaims);
    }

    @Test
    public void testBeforeManageClaimCreate() {
        when(commonFunctions.convertManageClaimsToClaims(manageClaims)).thenReturn(claims);
        when(complaintDao.getComplaintBasedOnCode(manageClaims.getComplaintCode())).thenReturn(result);
        handler.beforeManageClaimCreate(manageClaims);
    }

    @Test
    public void testBeforeManageClaimCreateCompanyCodePresent() {
        manageClaims.setContactPersonCode("C234567");
        manageClaims.setPersonResponsibleCode("C345");
        when(commonFunctions.convertManageClaimsToClaims(manageClaims)).thenReturn(claims);
        when(configurationService.validateSupplierContactPerson(manageClaims.getContactPersonCode())).thenReturn(partner);
        when(configurationService.validatePersonResponsibleCode(manageClaims.getPersonResponsibleCode())).thenReturn(partner);
        when(complaintDao.getComplaintBasedOnCode(manageClaims.getComplaintCode())).thenReturn(result);
        List<Row> rowvalues = new ArrayList<>();
        row.put("ID", "ComplaintID");
        opt = Optional.of(row);
        rowvalues.add(row);
        when(result.list()).thenReturn(rowvalues);
        when(result.first()).thenReturn(opt);
        handler.beforeManageClaimCreate(manageClaims);
    }

    @Test(expected = ServiceException.class)
    public void testBeforeManageClaimCreateException() {
        manageClaims.setComplaintCode("");
        when(commonFunctions.convertManageClaimsToClaims(manageClaims)).thenReturn(claims);

        handler.beforeManageClaimCreate(manageClaims);
    }

    @Test
    public void testAfterManageClaimCreateUpdate() {
        when(commonFunctions.convertManageClaimsToClaims(manageClaims)).thenReturn(claims);
        when(claimService.getClaimBasedOnId(claims.getId())).thenReturn(claims);
        handler.afterManageClaimCreateUpdate(manageClaims);
    }

    @Test
    public void testAfterManageClaimsUpdate() {
        when(claimService.getClaimBasedOnId(claims.getId())).thenReturn(claims);
        handler.afterManageClaimCreateUpdate(manageClaims);
    }

    @Test
    public void testBeforeManageClaimStatusUpdate(){
        when(claimService.getActiveClaimBasedOnId("123")).thenReturn(manageClaims);
        handler.beforeManageClaimStatusUpdate(manageClaims);
    }

    @Test
    public void testOnManageClaimStatusUpdate(){
        when(messageService.checkIfBOExists("123","CLM")).thenReturn("123");
        when(commonFunctions.convertManageClaimsToClaims(manageClaims)).thenReturn(claims);
        handler.onManageClaimStatusUpdate(manageClaims);
    }
}
