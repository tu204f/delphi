(******************************************************************************)
(* Параметры закрытия позиции, удержание позции и время задержки после убытка *)
(******************************************************************************)
unit Lb.ParamClose;

interface

type
  ///<summary>
  /// Параметры закрытие и удержания позции
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
