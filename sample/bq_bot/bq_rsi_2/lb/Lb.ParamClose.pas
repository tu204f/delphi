(******************************************************************************)
(* ��������� �������� �������, ��������� ������ � ����� �������� ����� ������ *)
(******************************************************************************)
unit Lb.ParamClose;

interface

type
  ///<summary>
  /// ��������� �������� � ��������� ������
  ///</summary>
  TParamClose = class(TObject)
  private

  public
    constructor Create; virtual;
    destructor Destroy; override;
  end;

implementation

{ TParamClose }

constructor TParamClose.Create;
begin

end;

destructor TParamClose.Destroy;
begin

  inherited;
end;

end.
