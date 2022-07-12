package com.sap.ic.cmh.masterdata.unitofmeasure.service;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.sap.cds.Result;
import com.sap.ic.cmh.masterdata.unitofmeasure.persistency.UnitOfMeasureRepository;

import cds.gen.masterdataservice.UnitOfMeasures;

@Service
public class UnitOfMeasureServiceImpl implements UnitOfMeasureService {

	@Autowired
	UnitOfMeasureRepository unitOfMeasureRepository;

	@Override
	public UnitOfMeasures getUnitOfMeasureDetails(String unitCode) {
		Result fetchUnitOfMeasureCodeResult = unitOfMeasureRepository.getUnitOfMeasureDetails(unitCode);
		return fetchUnitOfMeasureCodeResult.first().isPresent()
				? fetchUnitOfMeasureCodeResult.listOf(UnitOfMeasures.class).get(0)
				: null;
	}

}
