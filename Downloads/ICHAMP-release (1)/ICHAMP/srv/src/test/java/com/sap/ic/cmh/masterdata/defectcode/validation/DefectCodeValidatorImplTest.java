package com.sap.ic.cmh.masterdata.defectcode.validation;

import cds.gen.masterdataservice.DefectCodes;
import cds.gen.masterdataservice.DefectGroups;
import com.sap.cds.Result;
import com.sap.cds.Row;
import com.sap.cds.Struct;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.messages.Message;
import com.sap.cds.services.messages.Messages;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.ic.cmh.masterdata.common.persistency.MasterDataDao;
import com.sap.ic.cmh.utils.datavalidation.DataValidatorImpl;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;


public class DefectCodeValidatorImplTest {

    @InjectMocks
    private DefectCodeValidatorImpl validator;
    @Mock
    private MasterDataDao masterDataDao;

    @Mock
    private DataValidatorImpl dataValidator;
    @Mock
    private PersistenceService mockDb;
    @Mock
    private Messages messages;

    @Mock
    private Message msg;
    @Mock
    Result result;

    private DefectCodes codes;
    private DefectGroups groups;

    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
        codes = Struct.create(DefectCodes.class);
        groups = Struct.create(DefectGroups.class);
        groups.setCode("1");
        groups.setDescription("test");
        codes.setCode("1");
        codes.setDescription("test");
        codes.setDefectGroupCode(groups.getCode());
    }
    @Test
    public void checkInputsSanitized_ValidationPass() {
        Optional<DefectCodes> emptyOpt = Optional.of(codes);
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.first(DefectCodes.class)).thenReturn(emptyOpt);
        when(masterDataDao.getDefectGroup(any(String.class))).thenReturn(result);
        when(result.first(DefectCodes.class)).thenReturn(emptyOpt);
        Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
        Mockito.when(msg.target(any(String.class))).thenReturn(msg);
        validator.checkInputsSanitized(codes);
    }

    @Test
    public void validateItemTypeCodeNullTest() {
        DefectGroups itemTypes = Struct.create(DefectGroups.class);
        itemTypes.setCode("202");
        itemTypes.setDescription("test");
        List<DefectGroups> itemTypesList=new ArrayList<>();
        itemTypesList.add(itemTypes);
        Optional<DefectGroups> emptyOpt = Optional.of(itemTypes);
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.first(DefectGroups.class)).thenReturn(emptyOpt);
        when(masterDataDao.getDefectGroup(any())).thenReturn(result);
        List<Row> rowValues = new ArrayList<>();
        Row row=Struct.create(Row.class);
        row.put("ID", "201");
        row.put("businessObjectType", "202");
        Optional<Row> opt = Optional.of(row);
        rowValues.add(row);
        when(result.first()).thenReturn(Optional.of(row));
        when(result.iterator()).thenReturn(rowValues.iterator());
        when(result.first()).thenReturn(Optional.of(row));
        validator.checkInputsSanitized(codes);
    }

    @Test
    public void validateItemTypeCodeTest() {
        DefectGroups itemTypes = Struct.create(DefectGroups.class);
        itemTypes.setCode("202");
        itemTypes.setDescription("test");
        List<DefectGroups> itemTypesList=new ArrayList<>();
        itemTypesList.add(itemTypes);
        Optional<DefectGroups> emptyOpt = Optional.of(itemTypes);
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.first(DefectGroups.class)).thenReturn(emptyOpt);
        when(masterDataDao.getDefectGroup(any())).thenReturn(null);
        List<Row> rowValues = new ArrayList<>();
        Row row=Struct.create(Row.class);
        row.put("ID", "201");
        row.put("businessObjectType", "202");
        Optional<Row> opt = Optional.of(row);
        rowValues.add(row);
        when(result.first()).thenReturn(Optional.of(row));
        when(result.iterator()).thenReturn(rowValues.iterator());
        when(result.first()).thenReturn(Optional.of(row));
        Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
        Mockito.when(msg.target(any(String.class))).thenReturn(msg);
        validator.checkInputsSanitized(codes);
    }

}
