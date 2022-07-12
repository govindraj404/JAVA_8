package com.sap.ic.cmh.masterdata.defectcode.service;

import com.sap.cds.Result;
import com.sap.ic.cmh.masterdata.defectcode.repository.DefectCodeRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import cds.gen.masterdataservice.DefectCodes;

@Service
public class DefectCodeServiceImpl implements DefectCodeService{

    @Autowired
    DefectCodeRepository defectCodeRepository;

    @Override
    public DefectCodes fetchDefectCode(String defectCode, String defectGroup) {
      Result defectCodeResult =  defectCodeRepository.fetchDefectCode(defectCode,defectGroup);
        return defectCodeResult.first().isPresent() ? defectCodeResult.listOf(DefectCodes.class).get(0)
        : null;
    }
    
}
