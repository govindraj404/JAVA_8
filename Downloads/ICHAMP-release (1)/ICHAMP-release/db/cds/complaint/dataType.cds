namespace com.sap.ic.cmh.complaint.dataType;

/* ~~~~~~~~~~~~~~~~~~~~~~ START OF Complaint.cds ~~~~~~~~~~~~~~~~~~~  */

type Identifier     : String(30);
type Type           : String(20);
type Status         : String(20);
type Description    : String(100);
type Number         : String(40);
type Quantity       : Decimal(5, 2);
type Unit           : String(10);
type Note           : String(500);
type CreationType   : String(10);
type ContactPerson  : String(50);
type totalSubLetCost: Decimal(23, 2);
type TotalLaborHour : Decimal(10, 2);
type HideCost       : Boolean;
type UpdateRestricted : Boolean;
type Email : String(80);

/* ~~~~~~~~~~~~~~~~~~~~~~~~~ END OF Complaint.cds ~~~~~~~~~~~~~~~~~~~~  */

/* ~~~~~~~~~~~~~~~~~~~~~~ START OF CostCollector.cds ~~~~~~~~~~~~~~~~~~~  */

type TotalCost : Decimal(23, 2);

/* ~~~~~~~~~~~~~~~~~~~~~~ END OF CostCollector.cds ~~~~~~~~~~~~~~~~~~~  */
