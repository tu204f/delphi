unit Lb.SysUtils;

interface

uses
  System.Classes,
  System.SysUtils,
  System.math,
  System.Threading,
  System.DateUtils,
  System.SyncObjs,
  System.Generics.Collections;

type
  TTypeBuySell = (
    tsNull, {��������� ����� ��������}
    tsBuy,
    tsSell
  );

  TDoubleList = TList<Double>;

type
  ///<summary>
  /// �����
  ///</summary>
  TCandel = record
    Time: Int64;    // ���� � �����
    Open: Double;   // ���� ��������
    High: Double;   // ������������ ����
    Low: Double;    // ����������� ����
    Close: Double;  // �������� ����
    Vol: Double;    // ����� ������� ������
  end;

  ///<summary>
  /// ������ ������
  ///</summary>
  TCandelList = TList<TCandel>;

  ///<summary>
  /// ��������� �����
  ///</summary>
  TStateMarket = class(TObject)
  private
    FAsk: Double;
    FBid: Double;
    FQty: Double;
    FCandels: TCandelList;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure SetPrice(const AAsk, ABid: Double);
    ///<summary>
    /// ����� ��� ������������ ����
    ///</summary>
    property Qty: Double read FQty write FQty;
    ///<summary>
    /// ���� �������� - ������� ����� ����� Qty
    ///</summary>
    property Ask: Double read FAsk;
    ///<summary>
    /// ���� ���������� - ������� ����� ����� Qty
    ///</summary>
    property Bid: Double read FBid;
    property Candels: TCandelList read FCandels;
  end;

  ///<summary>
  /// �������
  ///</summary>
  TPositionMarket = class(TObject)
    OpenTime: Int64;
    CloseTime: Int64;
    Side: TTypeBuySell;
    OpenPrice: Double;
    ClosePrice: Double;
    MovingPrice: Double;
    Qty: Double;
    Value: Double;
  end;

function GetCrossSide(ASide: TTypeBuySell): TTypeBuySell;
function GetStrToSide(ASide: TTypeBuySell): String;

implementation

function GetStrToSide(ASide: TTypeBuySell): String;
begin
  case ASide of
    tsNull: Result := 'N';
    tsBuy: Result := 'B';
    tsSell: Result := 'S';
  end;
end;

function GetCrossSide(ASide: TTypeBuySell): TTypeBuySell;
begin
  case ASide of
    tsBuy: Result := TTypeBuySell.tsSell;
    tsSell: Result := TTypeBuySell.tsBuy;
  else
    Result := ASide;
  end;
end;

{ TStateMarket }

constructor TStateMarket.Create;
begin
  FAsk := 0;
  FBid := 0;
  FCandels := TCandelList.Create;
end;

destructor TStateMarket.Destroy;
begin
  FreeAndNil(FCandels);
  inherited;
end;

procedure TStateMarket.SetPrice(const AAsk, ABid: Double);
begin
  FAsk := AAsk;
  FBid := ABid;
end;

end.
