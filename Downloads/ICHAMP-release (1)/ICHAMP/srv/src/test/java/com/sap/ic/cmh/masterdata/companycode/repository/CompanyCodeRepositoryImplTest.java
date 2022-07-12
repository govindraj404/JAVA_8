package com.sap.ic.cmh.masterdata.companycode.repository;

import cds.gen.complaintservice.Complaints;
import cds.gen.masterdataservice.CompanyCodes;
import cds.gen.masterdataservice.CompanyCodes_;
import com.sap.cds.Result;
import com.sap.cds.Struct;
import com.sap.cds.ql.cqn.CqnDelete;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.persistence.PersistenceService;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import static org.junit.Assert.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

public class CompanyCodeRepositoryImplTest {
    @InjectMocks
    @Autowired
    CompanyCodeRepositoryImpl companyCodeRepository;
    @Mock
    private PersistenceService mockDb;
    @Mock
    Result result;

    private final List<String> companyCodeIds = new ArrayList<>();
    private final List<CompanyCodes> companyCodesList = new ArrayList<>();
    private final List<Complaints> complaintsList = new ArrayList<>();

    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
        companyCodeIds.add("001");

        CompanyCodes companyCodes = Struct.create(CompanyCodes.class);
        companyCodes.setId("100");
        companyCodes.setCompanyCode("F001");
        companyCodesList.add(companyCodes);

        Complaints complaints = Struct.create(Complaints.class);
        complaints.setCompanyCodeId("100");
        complaintsList.add(complaints);

        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(mockDb.run(any(CqnDelete.class))).thenReturn(result);
    }

    @Test
    public void testGetCompanyCodeMap() {
        Map<String, String> companyCodeMap = new HashMap<>();
        companyCodeMap.put("100", "F001");
        when(result.listOf(CompanyCodes.class)).thenReturn(companyCodesList);

        assertEquals(companyCodeMap, companyCodeRepository.getCompanyCodeMap(companyCodeIds));
    }

    @Test
    public void testGetActiveComplaintsInCompanyCode() {
        List<String> activeComplaintsList = new ArrayList<>();
        activeComplaintsList.add("100");
        when(result.listOf(Complaints.class)).thenReturn(complaintsList);

        assertEquals(activeComplaintsList, companyCodeRepository.getActiveComplaintsInCompanyCode(companyCodeIds));
    }

    @Test
    public void testDeleteCompanyCodeList() {
        long deleteCount = 0;
        when(result.rowCount()).thenReturn(deleteCount);

        companyCodeRepository.deleteCompanyCodeList(companyCodeIds);
    }

    @Test
    public void testGetCompanyCodeBasedOnCode() {
    	when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
    	companyCodeRepository.getCompanyCodeIdBasedOnCode("1000");
    }

    @Test
    public void TestFetchCompanyCode(){
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        companyCodeRepository.fetchCompanyCode("0000","message", CompanyCodes_.class,any());
    }

    @Test
    public void TestFetchCompanyCodesBasedOnCode(){
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        companyCodeRepository.fetchCompanyCodesBasedOnCode("0000");
    }
}
