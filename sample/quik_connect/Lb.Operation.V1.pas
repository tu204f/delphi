(*******************************************************************************
  ������ �������� � ��������, � ����������� ���������, ��� ������ ����������
*******************************************************************************)
unit Lb.Operation.V1;

interface

uses
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Generics.Collections,
  Lb.Level;

type
  ///<summary>�������� ���������</summary>
  TOperationTrade = class(TObject)
  private
    FStepPrice: Integer;
    FMinStep: Double;
    FReferenceLevel: TLevel;
    FBuyLevel: TLevel;
    FSellLevel: TLevel;
  protected
    property ReferenceLevel: TLevel read FReferenceLevel;
    property BuyLevels: TLevel read FBuyLevel;
    property SellLevels: TLevel read FSellLevel;
  public
    constructor Create;
    destructor Destroy; override;
    ///<summary>����������� ��� ����</summary>
    property MinStep: Double write FMinStep;
    ///<summary>���������� �����</summary>
    property StepPrice: Integer write FStepPrice;
    procedure SetReferenceSecCode(const AReferencePrice, ALast, ABid, AOffer: Double);
  end;

implementation

{ TOperationTrade }

constructor TOperationTrade.Create;
begin
  FReferenceLevel := TLevel.Create;
  FBuyLevel := TLevel.Create;
  FSellLevel := TLevel.Create;
end;

destructor TOperationTrade.Destroy;
begin
  FreeAndNil(FSellLevel);
  FreeAndNil(FBuyLevel);
  FreeAndNil(FReferenceLevel);
  inherited;
end;

procedure TOperationTrade.SetReferenceSecCode(const AReferencePrice, ALast, ABid, AOffer: Double);
begin
  FReferenceLevel.Price := AReferencePrice;
end;

end.
