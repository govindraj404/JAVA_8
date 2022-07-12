package com.sap.ic.cmh.managesupplierissueprocess.handler;

import cds.gen.managesupplierissueprocessservice.Supplier8DProcesses;
import cds.gen.masterdataservice.BusinessPartners;
import com.sap.cds.Result;
import com.sap.cds.Row;
import com.sap.cds.Struct;
import com.sap.cds.services.ServiceException;
import com.sap.cds.services.cds.CdsCreateEventContext;
import com.sap.ic.cmh.auditlog.AuditLogHelper;
import com.sap.ic.cmh.auditlog.AuditLogDifference;
import com.sap.ic.cmh.complaint.persistency.ComplaintsDao;
import com.sap.ic.cmh.configuration.service.ConfigurationService;
import com.sap.ic.cmh.supplierissueprocess.handler.EightDHandler;
import com.sap.ic.cmh.supplierissueprocess.service.EightDService;
import com.sap.ic.cmh.utils.CommonFunctions;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;
import static org.mockito.ArgumentMatchers.any;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import static org.mockito.Mockito.when;

public class ManageSupplierIssueProcessHandlerTest {

    @InjectMocks
    @Autowired
    ManageSupplierIssueProcessHandler handler;

    @Mock
    EightDHandler eightDHandler;

    @Mock
    CommonFunctions commonFunctions;

    @Mock
    ConfigurationService configurationService;

    @Mock
    ComplaintsDao complaintDao;

    @Mock
    Result result;
    @Mock
    CdsCreateEventContext context;

    @Mock
	EightDService eightDService;

    @Mock
    AuditLogHelper auditLogHelper;

    @Mock 
    private AuditLogDifference auditLogDifference;

    private Supplier8DProcesses manageSupplier8DProcesses;

    private Row row;
    private Optional<Row> opt;

    cds.gen.supplierissueprocessservice.Supplier8DProcesses supplierIssueProcess;

    private BusinessPartners partner;


    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);

        partner = Struct.create(BusinessPartners.class);
        partner.setId("1234");

        row = Struct.create(Row.class);

        manageSupplier8DProcesses = Struct.create(Supplier8DProcesses.class);

        manageSupplier8DProcesses.setSupplierIssueProcessesType("SDF");
        manageSupplier8DProcesses.setCompanyId("F987");
        manageSupplier8DProcesses.setComplaintId("FOO3");

        supplierIssueProcess = Struct.create(cds.gen.supplierissueprocessservice.Supplier8DProcesses.class);

        supplierIssueProcess.setId("F567");
        supplierIssueProcess.setSupplierId("D45676");
        supplierIssueProcess.setComplaintId("S3454");
    }

    @Test(expected = ServiceException.class)
    public void testBeforeManageSupplier8DCreateException() {
        manageSupplier8DProcesses.setComplaintCode("");
        when(commonFunctions
                .convertManageSupplierEightDToSupplierEightD(manageSupplier8DProcesses)).thenReturn(supplierIssueProcess);

        handler.beforeManageSupplier8DCreate(manageSupplier8DProcesses);
    }

    @Test
    public void testBeforeManageSupplier8DCreate() {
        manageSupplier8DProcesses.setComplaintCode("G5677");
        when(complaintDao.getComplaintBasedOnCode(manageSupplier8DProcesses.getComplaintCode())).thenReturn(result);
        when(commonFunctions
                .convertManageSupplierEightDToSupplierEightD(manageSupplier8DProcesses)).thenReturn(supplierIssueProcess);

        handler.beforeManageSupplier8DCreate(manageSupplier8DProcesses);
    }
    @Test
    public void testBeforeManageSupplier8DCreateCompPresent() {
        manageSupplier8DProcesses.setContactPersonCode("C234567");
        manageSupplier8DProcesses.setPersonResponsibleCode("C345");
        manageSupplier8DProcesses.setComplaintCode("G5677");
        when(complaintDao.getComplaintBasedOnCode(manageSupplier8DProcesses.getComplaintCode())).thenReturn(result);
        List<Row> rowvalues = new ArrayList<>();
        row.put("ID", "ComplaintID");
        opt = Optional.of(row);
        rowvalues.add(row);
        when(result.list()).thenReturn(rowvalues);
        when(result.first()).thenReturn(opt);
        when(configurationService.validateSupplierContactPerson(manageSupplier8DProcesses.getContactPersonCode())).thenReturn(partner);
        when(configurationService.validatePersonResponsibleCode(manageSupplier8DProcesses.getPersonResponsibleCode())).thenReturn(partner);

        when(commonFunctions
                .convertManageSupplierEightDToSupplierEightD(manageSupplier8DProcesses)).thenReturn(supplierIssueProcess);

        handler.beforeManageSupplier8DCreate(manageSupplier8DProcesses);
    }


    @Test
    public void testOnManageSupplier8DCreate(){
        when(commonFunctions
                .convertManageSupplierEightDToSupplierEightD(manageSupplier8DProcesses)).thenReturn(supplierIssueProcess);
        handler.onManageSupplier8DCreate(manageSupplier8DProcesses,context);
    }

    @Test
    public void testAfterManageSupplier8DCreate() {
        when(eightDService.getEightDBasedOnId(any())).thenReturn(supplierIssueProcess);
        handler.afterManageSupplier8DCreate(manageSupplier8DProcesses);
    }


    // @Test
    // public void testLogUpsert() {
    //     List<Supplier8DProcesses> data = new ArrayList<>();
    //     data.add(manageSupplier8DProcesses);
    //     handler.logUpsert(data);
    // }
}
