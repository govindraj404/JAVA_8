namespace com.sap.ic.cmh.returnPurchaseOrder.dataType;

/* ~~~~~~~~~~~~~~~~~~~~~~ START OF ReturnPurchaseOrder.cds ~~~~~~~~~~~~~~~~~~~  */

type Identifier     : String(30);
type Type           : String(20);
type ItemNumber     : String(30);
type Quantity       : Decimal(5, 2);
type Unit           : String(5);
type ContactPerson  : String(50);
type MovementType   : String(50);
type FieldControl   : Integer;
type UpdateRestricted : Boolean;

/* ~~~~~~~~~~~~~~~~~~~~~~~~~ END OF ReturnPurchaseOrder.cds ~~~~~~~~~~~~~~~~~~~~  */