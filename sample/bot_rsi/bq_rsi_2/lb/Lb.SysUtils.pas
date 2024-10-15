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
  TTypeSide = (
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
    Time: Integer;  // ���� � �����
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

implementation

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
