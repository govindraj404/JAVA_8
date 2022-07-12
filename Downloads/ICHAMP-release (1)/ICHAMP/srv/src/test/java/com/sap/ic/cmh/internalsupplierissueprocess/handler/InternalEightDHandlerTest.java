package com.sap.ic.cmh.internalsupplierissueprocess.handler;

import com.sap.cds.Struct;
import com.sap.ic.cmh.businessobjects.service.BusinessObjectService;
import com.sap.ic.cmh.supplierissueprocess.handler.EightDHandler;
import com.sap.ic.cmh.utils.CommonFunctions;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;

import static org.mockito.Mockito.when;

import cds.gen.supplierissueprocessinternalservice.Supplier8DProcesses;

public class InternalEightDHandlerTest {

    @InjectMocks
    @Autowired
    InternalEightDHandler handler;

    @Mock
    EightDHandler eightDHandler;

    @Mock
    CommonFunctions commonFunctions;
    
    @Mock
	BusinessObjectService businessObjectService;
    private Supplier8DProcesses manageSupplier8DProcesses;
    

    cds.gen.supplierissueprocessservice.Supplier8DProcesses supplierIssueProcess;


     @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);

        manageSupplier8DProcesses = Struct.create(Supplier8DProcesses.class);

        manageSupplier8DProcesses.setSupplierIssueProcessesType("SDF");
        manageSupplier8DProcesses.setCompanyId("F987");
        manageSupplier8DProcesses.setComplaintId("FOO3");

        supplierIssueProcess = Struct.create(cds.gen.supplierissueprocessservice.Supplier8DProcesses.class);

        supplierIssueProcess.setId("F567");
        supplierIssueProcess.setSupplierId("D45676");
        supplierIssueProcess.setComplaintId("S3454");
    }

       @Test
    public void testOnManageSupplier8DProcessesUpdate() {
        when(commonFunctions
                .convertInternalSupplierEightDToSupplierEightD(manageSupplier8DProcesses)).thenReturn(supplierIssueProcess);

        handler.onManageSupplier8DProcessesUpdate(manageSupplier8DProcesses);
    }


    @Test
    public void testAfterManageSupplier8DProcessesUpdate() {
        handler.afterManageSupplier8DProcessesUpdate(manageSupplier8DProcesses);

    }
    
}
