package com.sap.ic.cmh.masterdata.defectgroup.service;

import com.sap.cds.Result;
import com.sap.ic.cmh.masterdata.defectgroup.repository.DefectGroupRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import cds.gen.masterdataservice.DefectGroups;

@Service
public class DefectGroupServiceImpl implements DefectGroupService {
    
    @Autowired
    DefectGroupRepository defectGroupRepository;

    @Override
    public DefectGroups fetchDefectGroupCode(String defectGroupCode) {
      Result defectGroupResult =  defectGroupRepository.fetchDefectGroupCode(defectGroupCode);
        return defectGroupResult.first().isPresent() ? defectGroupResult.listOf(DefectGroups.class).get(0)
        : null;
    }
    
}
