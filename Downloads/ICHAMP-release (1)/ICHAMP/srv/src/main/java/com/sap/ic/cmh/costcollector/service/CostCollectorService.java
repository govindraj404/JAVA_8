package com.sap.ic.cmh.costcollector.service;

import cds.gen.complaintservice.Complaints;
import cds.gen.costcollectorservice.CostCollectors;

import java.math.BigDecimal;
import java.util.List;
import java.util.Map;

public interface CostCollectorService {
    public CostCollectors insertCostCollector(CostCollectors costCollector);

    public CostCollectors updateCostCollector(CostCollectors costCollector);

    public CostCollectors deleteCostCollector(CostCollectors costCollector);

    public List<CostCollectors> selectTransferToClaimCostCollector(String complaintId);

    public List<CostCollectors> selectAllCostCollector(String complaintId) ;

    public Map<String,Object> createExchangeRateDTO(String rateType, String fromCurrency, String toCurrency);

    public BigDecimal calculateExchangeRateAndConvertCost(CostCollectors costCollector, Complaints complaint);

    public String fetchConfiguredDestination(String companyID, String claimCode);

    public CostCollectors getCostCollector(String id);

    public BigDecimal convertQuantityOfFR(CostCollectors costCollector);

}