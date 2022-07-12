package com.sap.ic.cmh.masterdata.unitofmeasure.service;

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
import com.sap.ic.cmh.masterdata.unitofmeasure.persistency.UnitOfMeasureRepositoryImpl;

import cds.gen.masterdataservice.UnitOfMeasures;
import cds.gen.qualitynotificationservice.QualityNotifications;

public class UnitOfMeasureServiceTest {

	@InjectMocks
	@Autowired
	UnitOfMeasureServiceImpl unitOfMeasureServiceImpl;
	@Mock
	UnitOfMeasureRepositoryImpl unitOfMeasureRepositoryImpl;

	@Mock
	Result result;

	private Row row;
	private Optional<Row> opt;

	private UnitOfMeasures unitOfMeasures;
	private List<UnitOfMeasures> unitOfMeasuresList = new ArrayList<>();

	@Before
	public void beforeClass() {
		MockitoAnnotations.openMocks(this);

		unitOfMeasures = Struct.create(UnitOfMeasures.class);
		unitOfMeasures.setCode("ST");
		unitOfMeasures.setISOCode("PCE");

		unitOfMeasuresList.add(unitOfMeasures);
		
		row = Struct.create(Row.class);
	}

	@Test
	public void testGetUnitOfMeasureDetails() {

		List<Row> rowvalues = new ArrayList<>();
		row.put("code", "ST");
		row.put("ISOCode", "PCE");
		opt = Optional.of(row);
		rowvalues.add(row);
		when(result.list()).thenReturn(rowvalues);
		when(result.first()).thenReturn(opt);
		when(result.listOf(UnitOfMeasures.class)).thenReturn(unitOfMeasuresList);
		when(unitOfMeasureRepositoryImpl.getUnitOfMeasureDetails(unitOfMeasures.getCode())).thenReturn(result);
		unitOfMeasureServiceImpl.getUnitOfMeasureDetails(unitOfMeasures.getCode());

	}
	
	@Test
	public void testGetUnitOfMeasureDetailsNull() {
		when(unitOfMeasureRepositoryImpl.getUnitOfMeasureDetails(unitOfMeasures.getCode())).thenReturn(result);
		unitOfMeasureServiceImpl.getUnitOfMeasureDetails(unitOfMeasures.getCode());

	}

}
