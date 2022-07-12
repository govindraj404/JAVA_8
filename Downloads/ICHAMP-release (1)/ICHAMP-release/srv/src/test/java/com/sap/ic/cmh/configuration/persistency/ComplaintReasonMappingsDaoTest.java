package com.sap.ic.cmh.configuration.persistency;

import cds.gen.configurationservice.ComplaintReasonMappings;

import com.sap.cds.Result;
import com.sap.cds.Struct;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.ic.cmh.masterdata.distributionchannel.persistency.DistributionChannelRepository;
import com.sap.ic.cmh.masterdata.division.persistency.DivisionRepository;
import com.sap.ic.cmh.masterdata.salesorganization.persistency.SalesOrganizationRepository;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.util.Optional;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

public class ComplaintReasonMappingsDaoTest {
    @InjectMocks
    ComplaintReasonMappingsDao salesAreaItemCategoryReasonMapDao;

    @Mock
    PersistenceService db;
    @Mock
    Result result;
    @Mock
    Runnable run;

    private ComplaintReasonMappings salesAreaItemCategoryReasonMaps;

    @Mock
    DivisionRepository divisionRepository;

    @Mock
    DistributionChannelRepository distributionChannelRepository;

    @Mock
    SalesOrganizationRepository salesOrganizationRepository;

    @Mock
    ItemCategoriesDao itemCategoriesDao;

    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
        salesAreaItemCategoryReasonMaps = Struct.create(ComplaintReasonMappings.class);
        salesAreaItemCategoryReasonMaps.setSalesOrganizationId("5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7");
        salesAreaItemCategoryReasonMaps.setDistributionChannelId("5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7");
        salesAreaItemCategoryReasonMaps.setDivisionId("5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7");
        salesAreaItemCategoryReasonMaps.setItemCategoryId("5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7");
        salesAreaItemCategoryReasonMaps.setComplaintReasonId("5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7");
    }

    @Test
    public void getSalesAreaItemCategoryReasonTest(){;
        salesAreaItemCategoryReasonMapDao.getComplaintReasonMapOrderByIdentifier();
    }

    @Test
    public void getSalesAreaItemCategoryReasonMapTest(){
        salesAreaItemCategoryReasonMapDao.getComplaintReasonMapBasedOnAttributeIDs(
                "5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7",
                "5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7",
                "5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7",
                "5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7",
                "5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7");

    }

    @Test
    public void getItemCategoryTest(){
        itemCategoriesDao.getComplaintItemCategory(
                "5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7");

    }

    @Test
    public void getSalesOrgIdTest(){
        salesOrganizationRepository.getSalesOrganizationById(
                "5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7");

    }

    @Test
    public void getDistributionChannelIDTest(){
        distributionChannelRepository.getDistributionChannelById(
                "5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7");

    }

    @Test
    public void getDivisionIDTest(){
        divisionRepository.getDivisionIDandSalesIDById(
                "5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7");

    }
    @Test
    public void getComplaintReasonMapDetailsBasedOnIdTest(){
        salesAreaItemCategoryReasonMapDao.getComplaintReasonMapDetailsBasedOnId("complaintReasonId");
    }
}
