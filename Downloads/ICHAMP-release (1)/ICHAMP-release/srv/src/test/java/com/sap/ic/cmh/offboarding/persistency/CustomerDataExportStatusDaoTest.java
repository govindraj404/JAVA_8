package com.sap.ic.cmh.offboarding.persistency;

import cds.gen.com.sap.ic.cmh.customerdataexportstatus.CustomerDataExportStatuses;
import com.sap.cds.Result;
import com.sap.cds.Row;
import com.sap.cds.Struct;
import com.sap.cds.ql.cqn.CqnInsert;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.persistence.PersistenceService;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

public class CustomerDataExportStatusDaoTest {
    @InjectMocks
    CustomerDataExportStatusDao dao;
    @Mock
    PersistenceService db;
    @Mock
    Result result;
    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);}

    @Test
    public void createCustomerDataExportStatusTest(){
        CustomerDataExportStatuses coExportStatuses= Struct.create(CustomerDataExportStatuses.class);
        coExportStatuses.setStatus("test1");
        coExportStatuses.setId("test1");
        List<CustomerDataExportStatuses> list=new ArrayList<>();
        list.add(coExportStatuses);
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        when(result.listOf(CustomerDataExportStatuses.class)).thenReturn(list);
        List<Row> rowValues = new ArrayList<>();
        Row row=Struct.create(Row.class);
        row.put("ID", "test1");
        row.put("status", "test1");
        Optional<Row> opt = Optional.of(row);
        rowValues.add(row);
        Optional<CustomerDataExportStatuses> optional=Optional.of(coExportStatuses);
        when(db.run(any(CqnInsert.class))).thenReturn(result);
        when(result.first(CustomerDataExportStatuses.class)).thenReturn(optional);
        dao.createCustomerDataExportStatus(coExportStatuses);
    }
    @Test
    public void updateCustomerDataExportStatusTest(){
        CustomerDataExportStatuses coExportStatuses= Struct.create(CustomerDataExportStatuses.class);
        coExportStatuses.setStatus("test1");
        coExportStatuses.setId("test1");
        dao.updateCustomerDataExportStatus(coExportStatuses);
    }
    @Test
    public void getCustomerDataExportStatusTest(){
        CustomerDataExportStatuses coExportStatuses= Struct.create(CustomerDataExportStatuses.class);
        coExportStatuses.setStatus("test1");
        coExportStatuses.setId("test1");
        List<CustomerDataExportStatuses> list=new ArrayList<>();
        list.add(coExportStatuses);
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        when(result.listOf(CustomerDataExportStatuses.class)).thenReturn(list);
        List<Row> rowValues = new ArrayList<>();
        Row row=Struct.create(Row.class);
        row.put("ID", "test1");
        row.put("status", "test1");
        Optional<Row> opt = Optional.of(row);
        rowValues.add(row);
        Optional<CustomerDataExportStatuses> optional=Optional.of(coExportStatuses);
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        when(result.first(CustomerDataExportStatuses.class)).thenReturn(optional);
        dao.getCustomerDataExportStatus("test1");
    }
}
