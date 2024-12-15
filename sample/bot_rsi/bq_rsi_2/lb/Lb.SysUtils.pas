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
    FFirstCandelTime: Int64;
    FIsNewCandel: Boolean;
    FOnNewCandel: TNotifyEvent;
  protected
    procedure DoNewCandel;
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
  public
    procedure SetUpDataCandels;
    property Candels: TCandelList read FCandels;
    property OnNewCandel: TNotifyEvent write FOnNewCandel;
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
  FFirstCandelTime := 0;
end;

destructor TStateMarket.Destroy;
begin
  FreeAndNil(FCandels);
  inherited;
end;

procedure TStateMarket.DoNewCandel;
begin
  if Assigned(FOnNewCandel) then
    FOnNewCandel(Self);
end;

procedure TStateMarket.SetPrice(const AAsk, ABid: Double);
begin
  FAsk := AAsk;
  FBid := ABid;
end;

procedure TStateMarket.SetUpDataCandels;
var
  xFirstCandelTime: Int64;
begin
  FIsNewCandel := False;
  if FCandels.Count > 0 then
  begin
    xFirstCandelTime := FCandels[0].Time;
    if FFirstCandelTime = 0 then
    begin
      FFirstCandelTime := xFirstCandelTime;
    end
    else
    begin
      FIsNewCandel := FFirstCandelTime <> xFirstCandelTime;
      if FIsNewCandel then
      begin
        FFirstCandelTime := xFirstCandelTime;
        DoNewCandel;
      end;
    end;
  end;
end;

end.
