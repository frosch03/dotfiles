#!/usr/bin/env bash
#
# Input parser for status bar

# config
. $(dirname $0)/frogs_lemonbar_config

# min init
irc_n_high=0
title="%{F${color_head} B${color_sec_b2}}${sep_right}%{F${color_head} B${color_sec_b2}%{T2} ${icon_prog} %{F${color_sec_b2} B-}${sep_right}%{F- B- T1}"

# parser
while read -r line ; do
    case $line in
        SYS*)
            # conky=, 0=wday, 1=mday, 2=month, 3=time, 4=cpu, 5=temp, 6=ram, 7=ram unit, 8=disk used perc, 9-10=up/down eth0, 11-12=up/down wlan0, 13=power plugged in, 14=BAT0, 15=BAT1
            sys_arr=(${line#???})
            # date
            if [ ${res_w} -gt 1024 ]; then
                date="${sys_arr[0]} ${sys_arr[1]} ${sys_arr[2]}"
            else
                date="${sys_arr[1]} ${sys_arr[2]}"
            fi
            date="%{F${color_sec_b1}}${sep_left}%{F${color_icon} B${color_sec_b1}} %{T2}${icon_clock}%{F- T1} ${date}"
            # time
            time="%{F${color_head}}${sep_left}%{F${color_back} B${color_head}} ${sys_arr[3]} %{F- B-}"
            # cpu
            if [ ${sys_arr[4]} -gt ${cpu_alert} ]; then
                cpu_cback=${color_cpu}; cpu_cicon=${color_back}; cpu_cfore=${color_back};
            else
                cpu_cback=${color_sec_b2}; cpu_cicon=${color_icon}; cpu_cfore=${color_fore};
            fi
            cpu="%{F${cpu_cback}}${sep_left}%{F${cpu_cicon} B${cpu_cback}} %{T2}${icon_cpu}%{F${cpu_cfore} T1} ${sys_arr[4]} %"
            # mem
            coretemp="%{F${cpu_cicon}}${sep_l_left} %{F${cpu_cfore} T1} ${sys_arr[5]}°C"
            # mem
            mem="%{F${cpu_cicon}}${sep_l_left} %{T2}${icon_mem}%{F${cpu_cfore} T1} ${sys_arr[6]} ${sys_arr[7]}"
            # disk /
            diskused="%{F${color_sec_b1}}${sep_left}%{F${color_icon} B${color_sec_b1}} %{T2}${icon_hd}%{F- T1} ${sys_arr[8]}%"
            # eth
            if [ "${sys_arr[9]}" == "down" ]; then
                ethd_v="×"; ethu_v="×";
                eth_cback=${color_sec_b1}; eth_cicon=${color_disable}; eth_cfore=${color_disable};
                net_seperator="%{F${eth_cback}}${sep_left}"
            else
                ethd_v=${sys_arr[9]}K; ethu_v=${sys_arr[10]}K;
                if [ ${ethd_v:0:-3} -gt ${net_alert} ] || [ ${ethu_v:0:-3} -gt ${net_alert} ]; then
                    eth_cback=${color_net}; eth_cicon=${color_back}; eth_cfore=${color_back};
                else
                    eth_cback=${color_sec_b2}; eth_cicon=${color_icon}; eth_cfore=${color_fore};
                fi
                net_seperator="%{F${eth_cicon}}${sep_l_left}"
            fi
            ethd="${net_seperator}%{F${eth_cicon} B${eth_cback}} %{T2}${icon_eth}%{T2}${icon_dl}%{F${eth_cfore} T1} ${ethd_v}" # "%{F${eth_cback}}${sep_left}%{F${eth_cicon} B${eth_cback}} %{T2}${icon_eth}%{T2}${icon_dl}%{F${eth_cfore} T1} ${ethd_v}"
            ethu="%{T2}${icon_ul}%{F${eth_cfore} T1} ${ethu_v}" # "%{F${eth_cicon}}${sep_l_left} %{T2}${icon_ul}%{F${eth_cfore} T1} ${ethu_v}"
            # wlan
            if [ "${sys_arr[11]}" == "down" ]; then
                wland_v="×"; wlanu_v="×";
                wlan_cback=${color_sec_b1}; wlan_cicon=${color_disable}; wlan_cfore=${color_disable};
                net_seperator="%{F${color_icon}}${sep_left}"
            else
                wland_v=${sys_arr[11]}K; wlanu_v=${sys_arr[12]}K;
                if [[ ${wland_v:0:-3} -gt ${net_alert} ]] || [[ ${wlanu_v:0:-3} -gt ${net_alert} ]]; then
                    wlan_cback=${color_net}; wlan_cicon=${color_back}; wlan_cfore=${color_back};
                else
                    wlan_cback=${color_sec_b2}; wlan_cicon=${color_icon}; wlan_cfore=${color_fore};
                fi
                net_seperator="%{F${color_icon}}${sep_left}"
            fi
            wland="%{F${wlan_cback}}${sep_left}%{F${wlan_cicon} B${wlan_cback}} %{T2}${icon_wlan}%{T2}${icon_dl}%{F${wlan_cfore} T1} ${wland_v}"
            wlanu="%{T2}${icon_ul}%{F${wlan_cfore} T1} ${wlanu_v}" # "%{F${wlan_cicon}}${sep_l_left} %{T2}${icon_ul}%{F${wlan_cfore} T1} ${wlanu_v}"
            # Battery 0
            if [ ! "${sys_arr[13]}" == "off" ]; then
                bat0_cfore=${color_batM}
                bat0_cback=${color_batLoad}
            else
                bat0_cfore=${color_disable}
                if [ ${sys_arr[14]} -gt 50 ]; then
                    bat0_cback=${color_batH}
                elif [ ${sys_arr[14]} -gt 30 ]; then
                    bat0_cback=${color_batU}
                elif [ ${sys_arr[14]} -gt 15 ]; then
                    bat0_cback=${color_batM}
                else
                    bat0_cback=${color_batL}
                fi
            fi
            bat0="%{F${bat0_cback}}${sep_left}%{F${bat0_cfore} B${bat0_cback}}${icon_battery} 0:${sys_arr[14]} "
            # Battery 1
            if [ ! "${sys_arr[13]}" == "off" ]; then
                bat1_cback=${color_batLoad}
                bat1_cfore=${color_batM}
            else
                bat1_cfore=${color_disable}
                if [ ${sys_arr[15]} -gt 50 ]; then
                    bat1_cback=${color_batH}
                elif [ ${sys_arr[15]} -gt 30 ]; then
                    bat1_cback=${color_batU}
                elif [ ${sys_arr[15]} -gt 15 ]; then
                    bat1_cback=${color_batM}
                else
                    bat1_cback=${color_batL}
                fi
            fi
            bat1="%{B${bat1_cback}}1:${sys_arr[15]} "
            ;;
        VOL*)
            # Volume
            vol="%{F${color_sec_b2}}${sep_left}%{F${color_icon} B${color_sec_b2}} %{T2}${icon_vol}%{F- T1} ${line#???}"
            ;;
        GMA*)
            # Gmail
            gmail="${line#???}"
            if [ "${gmail}" != "0" ]; then
                mail_cback=${color_mail}; mail_cicon=${color_back}; mail_cfore=${color_back}
            else
                mail_cback=${color_sec_b1}; mail_cicon=${color_icon}; mail_cfore=${color_fore}
            fi
            gmail="%{F${mail_cback}}${sep_left}%{F${mail_cicon} B${mail_cback}} %{T2}${icon_mail}%{F${mail_cfore} T1} ${gmail}"
            ;;
        IRC*)
            # IRC highlight (script irc_warn)
            if [ "${line#???}" != "0" ]; then
                ((irc_n_high++)); irc_high="${line#???}";
                irc_cback=${color_chat}; irc_cicon=${color_back}; irc_cfore=${color_back}
            else
                irc_n_high=0; [ -z "${irc_high}" ] && irc_high="none";
                irc_cback=${color_sec_b2}; irc_cicon=${color_icon}; irc_cfore=${color_fore}
            fi
            irc="%{F${irc_cback}}${sep_left}%{F${irc_cicon} B${irc_cback}} %{T2}${icon_chat}%{F${irc_cfore} T1} ${irc_n_high} %{F${irc_cicon}}${sep_l_left} %{T2}${icon_contact}%{F${irc_cfore} T1} ${irc_high}"
            ;;
        MPD*)
            # Music
            mpd_arr=(${line#???})
            if [ -z "${line#???}" ]; then
                song="none";
            elif [ "${mpd_arr[0]}" == "error:" ]; then
                song="mpd off";
            else
                song="${line#???}";
            fi
            mpd="%{F${color_sec_b2}}${sep_left}%{B${color_sec_b2}}${sep_left}%{F${color_icon} B${color_sec_b2}} %{T2}${icon_music}%{F${color_fore} T1}  ${song}"
            ;;
        WSP*)
            # I3 Workspaces
            wsp="%{F${color_back} B${color_head}} %{T2}${icon_os}%{T1}"
            set -- ${line#???}
            while [ $# -gt 0 ] ; do
                case $1 in
                    FOC*)
                        wsp="${wsp}%{F${color_head} B${color_wsp}}${sep_right}%{F${color_back} B${color_wsp} T1} ${1#???} %{F${color_wsp} B${color_head}}${sep_right}"
                        ;;
                    INA*|URG*|ACT*)
                        wsp="${wsp}%{F${color_disable} T1} ${1#???} "
                        ;;
                esac
                shift
            done
            ;;
        WIN*)
            # window title
            title=${line#???}
            #title=$(xprop -id ${line#???} | awk '/_NET_WM_NAME/{$1=$2="";print}' | cut -d'"' -f2)
            #title="%{F${color_head} B${color_sec_b2}}${sep_right}%{F${color_head} B${color_sec_b2}%{T2} ${icon_prog} %{F${color_sec_b2} B-}${sep_right}%{F- B- T1} ${title}"
            title="%{F${color_head} B${color_sec_b2}}${sep_right}%{F${color_head} B${color_sec_b2} T2} ${icon_prog} %{F${color_sec_b2} B-}${sep_right}%{F- B- T1} ${title}"
            ;;
    esac

    # And finally, output
    # printf "%s\n" "%{l}${wsp}${title} %{r}${mpd}${stab}${irc}${stab}${gmail}${stab}${cpu}${stab}${mem}${stab}${diskused}${stab}${diskh}${stab}${wland}${stab}${wlanu}${stab}${ethd}${stab}${ethu}${stab}${vol}${stab}${bat0}${bat1}${date}${stab}${time}"
    # without mpd:
    printf "%s\n" "%{S0}%{l}${wsp}${title} %{S1}%{r}${mpd}${stab}${ethd}${stab}${ethu}${stab}${wland}${stab}${wlanu}${stab}${gmail}${stab}${cpu}${stab}${coretemp}${stab}${mem}${stab}${diskused}${stab}${vol}${stab}${bat0}${bat1}${date}${stab}${time}"
    #printf "%s\n" "%{l}${wsp}${title}"
done
