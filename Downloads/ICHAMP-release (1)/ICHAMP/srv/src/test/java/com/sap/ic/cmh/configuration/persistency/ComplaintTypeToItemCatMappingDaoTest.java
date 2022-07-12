package com.sap.ic.cmh.configuration.persistency;

import cds.gen.configurationservice.ComplaintTypeToItemCategoryMappings;
import com.sap.cds.Struct;
import com.sap.cds.services.persistence.PersistenceService;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

public class ComplaintTypeToItemCatMappingDaoTest {

    @InjectMocks
    ComplaintTypeToItemCatMappingDao dao;

    @Mock
    PersistenceService db;

    ComplaintTypeToItemCategoryMappings complaintTypeToItemCategoryMappings;

    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
        complaintTypeToItemCategoryMappings = Struct.create(ComplaintTypeToItemCategoryMappings.class);
        complaintTypeToItemCategoryMappings.setSalesOrganizationId("100");
        complaintTypeToItemCategoryMappings.setDistributionChannelId("100");
        complaintTypeToItemCategoryMappings.setDivisionId("120");
        complaintTypeToItemCategoryMappings.setComplaintTypeId("1");
        complaintTypeToItemCategoryMappings.setItemCategoryId("2");
        complaintTypeToItemCategoryMappings.setId("100");
    }

    @Test
    public void getComplaintTypeToItemCatMappingIdentifierTest() {
        dao.getComplaintTypeToItemCatMappingIdentifier();
    }

    @Test
    public void getComplaintTypeToItemCatMappingBasedOnUniqueFieldsTest() {
        dao.getComplaintTypeToItemCatMappingBasedOnUniqueFields(complaintTypeToItemCategoryMappings);

    }

    @Test
    public void getComplaintTypeToItemCatBasedOnIdTest(){
        dao.getComplaintTypeToItemCatBasedOnId("complaintTypeToItemCatId");
    }
}
