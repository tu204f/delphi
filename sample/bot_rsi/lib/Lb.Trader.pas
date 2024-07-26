unit Lb.Trader;

interface

uses
  System.Classes,
  System.SysUtils,
  System.math,
  System.Threading,
  System.DateUtils,
  System.SyncObjs,
  System.Generics.Collections,
  System.Generics.Defaults,
  Lb.Bybit.SysUtils;

type
  TOrder = class;

  ///<summary>
  /// ��� ���������
  ///</summary>
  TType�riterion = (tcOpen, tcClose);

  ///<summary>
  /// �������� ������� �������
  ///</summary>
  T�riterion = class(TObject)
  public
    ///<summary>
    /// ����������� ��������
    ///</summary>
    Side: TTypeSide;

    ///<summary>
    /// �������� ����������
    ///</summary>
    RSI: Double;

    ///<summary>
    /// �������� ����������, ��� ��������� ��������
    ///</summary>
    ReActiveRSI: Double;

    ///<summary>
    /// ����������
    ///</summary>
    Qty: Double;

    ///<summary>
    /// ������������ ������������
    ///</summary>
    IsActive: Boolean;

    ///<summary>
    /// ��� ��������: ��������, ��������
    ///</summary>
    Type�riterion: TType�riterion;
  public
    constructor Create; virtual;
    procedure Default;
  end;
  T�riterionList = TObjectList<T�riterion>;

  ///<summary>������� �������� �� ��������</summary>
  TOrder = class(TObject)
  private

  public
    constructor Create; virtual;
    destructor Destroy; override;
  end;

implementation

///<summary>
/// ����������� �������� RSI,
/// ����� ������� �������� (AValue),
/// ���������� (AParam) - ���������� ������
///</summary>
function GetUpRSI(const AValue, AParam: Double): Boolean;
begin
  Result := AParam < AValue;
end;

///<summary>
/// ����������� �������� RSI,
/// ����� ������� �������� (AValue),
/// ���������� (AParam) - ����������� ������
///</summary>
function GetDownRSI(const AValue, AParam: Double): Boolean;
begin
  Result := AParam > AValue;
end;

{ T�riterion }

constructor T�riterion.Create;
begin

end;

procedure T�riterion.Default;
begin

end;

{ TOrder }

constructor TOrder.Create;
begin

end;

destructor TOrder.Destroy;
begin

  inherited;
end;



end.
