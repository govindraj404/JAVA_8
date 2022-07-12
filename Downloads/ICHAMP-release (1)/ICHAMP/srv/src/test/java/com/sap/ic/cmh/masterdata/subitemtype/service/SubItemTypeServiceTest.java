package com.sap.ic.cmh.masterdata.subitemtype.service;

import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;

import com.sap.cds.Result;
import com.sap.cds.Row;
import com.sap.cds.Struct;
import com.sap.ic.cmh.masterdata.subitemtype.repository.SubItemTypeRepositoryImpl;

import cds.gen.masterdataservice.Addresses;
import cds.gen.masterdataservice.SubItemTypes;

public class SubItemTypeServiceTest {

	@InjectMocks
	@Autowired
	SubItemTypeServiceImpl subItemTypeServiceImpl;

	@Mock
	Result result;
	@Mock
	Row row;
	@Mock
	SubItemTypeRepositoryImpl repo;

	private SubItemTypes subItemType;

	@Before
	public void beforeClass() {
		MockitoAnnotations.openMocks(this);
		subItemType = Struct.create(SubItemTypes.class);

		subItemType.setCode("10000001");
		subItemType.setDescription("Henry-Strasse");
		subItemType.setItemTypeCode("70839");
	}
	
	@Test
	public void testFetchSubItemTypes() {
		List<Row> rowvalues = new ArrayList<>();
        row.put("address", "ComplaintID");
        row.put("ID", "1");
        Optional<Row> optRow = Optional.of(row);
        List<SubItemTypes> subItemTypeList = new ArrayList<>();
        subItemTypeList.add(subItemType);
        rowvalues.add(row);
        Mockito.when(result.list()).thenReturn(rowvalues);
        Mockito.when(result.first()).thenReturn(optRow);
        Mockito.when(result.listOf(SubItemTypes.class)).thenReturn(subItemTypeList);
        when(repo.fetchSubItemTypes(subItemType.getCode())).thenReturn(result);
        subItemTypeServiceImpl.fetchSubItemTypes(subItemType.getCode());
	}

	@Test
	public void testFetchSubItemTypesNull() {
		when(repo.fetchSubItemTypes(subItemType.getCode())).thenReturn(result);
		subItemTypeServiceImpl.fetchSubItemTypes(subItemType.getCode());
	}

}
