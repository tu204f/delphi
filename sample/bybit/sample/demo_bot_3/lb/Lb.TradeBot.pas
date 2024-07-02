unit Lb.TradeBot;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  Lb.Bybit.SysUtils,
  Lb.Bybit.Kline;

type
  ///<summary>
  /// ������� �������
  ///</summary>
  TOnEventConditionTrade = procedure(ASender: TObject; ASide: TTypeSide) of object;

  ///<summary>
  /// ������ � ��������
  ///</summary>
  ///<remarks>
  /// TTypeCategory.tcLinear - �������� ������ �� �������� �������������
  ///</remarks>
  TTradeLine = class(TObject)
  private
    FSide: TTypeSide;
    FValueRSI: Double;
    FIsActive: Boolean;
  private

  protected
    procedure DoEventConditionTrade;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    ///<summary>
    /// ���������� �������� ����������
    ///</summary>
    procedure UpData(const AValueRSI: Double);
    ///<summary>
    /// ������� �������
    ///</summary>
    property IsActive: Boolean read FIsActive write FIsActive;
    ///<summary>
    /// ����������� ��������
    ///</summary>
    property Side: TTypeSide read FSide write FSide;
  end;


implementation

uses
  Lb.Setting,
  Lb.OperationTrade,
  Lb.Bybit.Trade;

{ TTradeLine }

constructor TTradeLine.Create;
begin
  FSide := TTypeSide.tsBuy;

end;

destructor TTradeLine.Destroy;
begin

  inherited;
end;

procedure TTradeLine.DoEventConditionTrade;
begin

end;

procedure TTradeLine.UpData(const AValueRSI: Double);
begin
  FValueRSI := AValueRSI;
end;

end.
