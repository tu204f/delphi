unit Lb.Custom.Trade;

interface

uses
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Generics.Collections,
  Lb.Level;

type
  ///<summary>�������� ���������</summary>
  ///<remarks>��� ������ ���� �������� � ����� �����������</remarks>
  TCustomTrade = class(TObject)
  private
    FMinStep: Double;
    FReferenceLevel: TLevel;
  protected
    property ReferenceLevel: TLevel read FReferenceLevel;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    ///<summary>����������� ��� ����</summary>
    property MinStep: Double read FMinStep write FMinStep;
    procedure SetReferenceSecCode(const AReferencePrice, ALast, ABid, AOffer: Double); virtual;
  public
    ///<summary>���������� ��������� � �������� ��������</summary>
    ///<returns>����������� ����� ���������</returns>
    function GetOrder(APrice: Double; AQuantity: Integer; ABuySell: Char): Integer; virtual;
    ///<summary>���������� ������ �������� ������</summary>
    function GetDeleteOrder(AOrderNo: Int64): Boolean; virtual;
    ///<summary>���������� ������� ������</summary>
    function GetMoveOrder(AOrderNo: Int64; APrice: Double; AQuantity: Integer; ABuySell: Char): Integer; virtual;
  end;

implementation

{ TCustomTrade }

constructor TCustomTrade.Create;
begin
  FReferenceLevel := TLevel.Create;
end;

destructor TCustomTrade.Destroy;
begin
  FreeAndNil(FReferenceLevel);
  inherited;
end;

procedure TCustomTrade.SetReferenceSecCode(const AReferencePrice, ALast, ABid,
  AOffer: Double);
begin
  {����������� �������� ���������}
end;

function TCustomTrade.GetOrder(APrice: Double; AQuantity: Integer; ABuySell: Char): Integer;
begin
  Result := -1;
  {��������� ������}
end;

function TCustomTrade.GetDeleteOrder(AOrderNo: Int64): Boolean;
begin
  Result := False;
  {�������� ������ �� ������}
end;

function TCustomTrade.GetMoveOrder(AOrderNo: Int64; APrice: Double; AQuantity: Integer; ABuySell: Char): Integer;
begin
  {���������� ������}
end;

end.
