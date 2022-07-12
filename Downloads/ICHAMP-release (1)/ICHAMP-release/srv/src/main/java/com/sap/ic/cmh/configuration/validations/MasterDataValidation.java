package com.sap.ic.cmh.configuration.validations;

import java.math.BigDecimal;
import com.sap.cds.ql.StructuredType;
import java.util.function.Function;

public interface MasterDataValidation {

    public void validatePlant(String plantId);

    public void validateMaterial(String materialId);

    public void validateCompanyCode(String companyId);

    public void validateSupplier(String supplierId);

    public void validatePurchaseOrg(String purchasingOrganizationId);

    public void validateQuantity(BigDecimal quantity);
    
    public void validUnitOfMeasure(String unit);

    public void validateSupplierPersonType(String businessPartnerId);

    public boolean validateBTPUser(String personResponsible);
    
    public boolean checkSalesOrganizationExist(String salesOrganizationId);
    
    public <E extends StructuredType<E>> void validateSalesOrganization(String salesOrganizationId, Class<E> targetClass, Function<E,Object> targetClassAttribute);
    
    public <E extends StructuredType<E>> void validateDistributeChannel( String distributionChannelId, String salesOrganizationId, Class<E> targetClass, Function<E,Object> targetClassAttribute);
    
    public <E extends StructuredType<E>> void validateDivision(String divisionId, String salesOrganizationId, Class<E> targetClass, Function<E,Object> targetClassAttribute);


}
