package com.sap.ic.cmh.masterdata.reason.service;

import com.sap.cds.Result;
import com.sap.ic.cmh.masterdata.reason.repository.RejectReasonRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import cds.gen.masterdataservice.Reasons;

@Service
public class RejectReasonServiceImpl implements RejectReasonService {

    @Autowired
    RejectReasonRepository rejectReasonRepository;

    @Override
    public Reasons fetchReasonBasedOnCode(String reasonCode) {
        Result reasonResult = rejectReasonRepository.fetchReasonBasedOnCode(reasonCode);
        return reasonResult.first().isPresent() ? reasonResult.listOf(Reasons.class).get(0) : null;
    }

}
