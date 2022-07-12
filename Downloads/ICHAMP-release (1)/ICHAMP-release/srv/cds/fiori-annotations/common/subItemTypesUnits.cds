using com.sap.ic.cmh as cmh from '../../../../db/cds/index';

@cds.autoexpose
annotate cmh.subItemTypeUnit.SubItemTypesUnits with {

	ID
	@UI.Hidden : true;

	companyCode
	@UI.Hidden : true;

	destination
	@UI.Hidden : true;

	unit
	@UI.Hidden : true;

    description
    @UI.Hidden : true;

    serviceMaterialID
    @UI.Hidden : true;
};