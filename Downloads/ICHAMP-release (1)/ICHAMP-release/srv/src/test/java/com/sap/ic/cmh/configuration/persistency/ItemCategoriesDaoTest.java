package com.sap.ic.cmh.configuration.persistency;

import com.sap.cds.Result;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.persistence.PersistenceService;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

public class ItemCategoriesDaoTest {

    @InjectMocks
    ItemCategoriesDao itemCategoriesDao;

    @Mock
    Result result;

    @Mock
    PersistenceService mockDb;

    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
    }

    @Test
    public void testGetComplaintItemCategory() {
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        itemCategoriesDao.getComplaintItemCategory("id");
    }

    @Test
    public void testGetItemCategoryBasedOnCode() {
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        itemCategoriesDao.getItemCategoryBasedOnCode("code");
    }

    @Test
    public void testGetItemCategoryDetails() {
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        itemCategoriesDao.getItemCategoryDetails();
    }

    @Test
    public void getItemCategoryDetailsBasedOnIdTest() {
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        itemCategoriesDao.getItemCategoryDetailsBasedOnId("3f6c9e0e-b3e0-41a3-8935-ece2bfa66af8");
    }
}



