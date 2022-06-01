modelo.coop <- coop |>
   dplyr::mutate(
      log_ativo_total = log(ativo_total),
      log_patrimonio_liquido = log(patrimonio_liquido),
      log_despesas_operacionais = log(despesas_operacionais - min(despesas_operacionais)),
      log_despesas_operacionais = dplyr::case_when(
         log_despesas_operacionais == -Inf ~ 0,
         log_despesas_operacionais != -Inf ~ log_despesas_operacionais
      ),
      log_receitas_operacionais = log(receitas_operacionais),
      log_sobras = log(sobras - min(sobras)),
      log_sobras = dplyr::case_when(
         log_sobras == -Inf ~ 0,
         log_sobras != -Inf ~ log_sobras
      ),
      log_pecld = log(pecld - min(pecld)),
      log_pecld = dplyr::case_when(
         log_pecld == -Inf ~ 0,
         log_pecld != -Inf ~ log_pecld
      ),
      filiacao = dplyr::case_when(
         filiacao != 0 ~ 1,
         filiacao == 0 ~ 0
      )
   ) |>
   dplyr::select(
      big_four, idade_em_2022, numero_agencias, total_de_cooperados,
      log_ativo_total, log_patrimonio_liquido, log_despesas_operacionais,
      log_receitas_operacionais, log_sobras, log_pecld, filiacao
   )