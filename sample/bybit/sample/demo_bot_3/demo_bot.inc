{$IFDEF DEBUG}
  {$DEFINE DB_LOG}
  {$DEFINE HISTORY_INDICATOR}
  {$DEFINE INSTRUMENT_PRICE}
  {$DEFINE VERTUAL_TRADE}
{$ENDIF}


{$IFDEF DB_LOG}
  //{$DEFINE ONE_TRADE}
  {$DEFINE DEBUG_TRADE}
{$ENDIF}


{$IFDEF HISTORY_INDICATOR}
  {$DEFINE SET_SHOW_GRID}
{$ENDIF}