# ROADMAP02 P0 smoke suite. Run from the repository root:
#   powershell -ExecutionPolicy Bypass -File tests\run_roadmap02.ps1
$exe = '.\bin\tyro.exe'
$cases = @(
    @('tests\roadmap02_p0_api_test.tyro', '--exit', 0),
    @('tests\roadmap02_p0_sound_test.tyro', '--exit', 0),
    @('tests\roadmap02_p0_feedback_test.tyro', '--exit:2', 0),
    @('tests\roadmap02_p0_robustness_test.tyro', '--exit', 0),
    @('tests\roadmap02_p0_watchdog_test.tyro', '--exit:5', 1),
    @('tests\syntax_error_audit.tyro', '--exit', 1),
    @('tests\test_cli_exit_script.tyro', '--exit', 0),
    @('demos\bounce_demo.tyro', '--exit:2', 0),
    @('demos\animated_demo.tyro', '--exit:2', 0),
    @('demos\sound.tyro', '--exit:2', 0),
    @('demos\first_game.tyro', '--exit:2', 0),
    @('tests\roadmap02_p1_scene_test.tyro', '--exit:3', 0),
    @('tests\roadmap02_p1_pause_test.tyro', '--exit:3', 0),
    @('tests\roadmap02_p1_restart_test.tyro', '--exit:3', 0),
    @('tests\roadmap02_p1_helpers_test.tyro', '--exit', 0)
)
$failed = 0
foreach ($c in $cases) {
    & $exe $c[0] $c[1] 2>&1 | Out-Null
    $code = $LASTEXITCODE
    if ($code -eq $c[2]) { $mark = 'PASS' } else { $mark = 'FAIL'; $failed++ }
    "$mark $($c[0]) exit=$code expected=$($c[2])"
}
"BANNER screenshot: tests\roadmap02_p0_banner_test.tyro --exit:2 --screenshot:1"
if ($failed -gt 0) { exit 1 }
'ALL ROADMAP02 P0 CHECKS PASSED'
