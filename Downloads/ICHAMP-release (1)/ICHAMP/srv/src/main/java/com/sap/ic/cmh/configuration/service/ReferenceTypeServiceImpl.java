package com.sap.ic.cmh.configuration.service;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import cds.gen.configurationservice.ReferenceTypes;
import com.sap.cds.Result;
import com.sap.ic.cmh.configuration.persistency.ReferenceTypeDao;

@Service
public class ReferenceTypeServiceImpl implements ReferenceTypeService {
	
	@Autowired
	ReferenceTypeDao referenceTypeDao;

    /**
	 * Get reference type
	 * 
	 * @public
	 */
	public Result getReferenceTypes() {
		return referenceTypeDao.getReferenceTypeDetails();
	}

	/**
	 * Get reference details based on ID
	 */
	@Override
	public ReferenceTypes getReferenceTypesDetails(String id) {
		Result referenceTypesResult = referenceTypeDao.getReferenceTypeDetailsBasedOnId(id);
		return referenceTypesResult.first().isPresent() ? referenceTypesResult.listOf(ReferenceTypes.class).get(0)
				: null;
	}
}
