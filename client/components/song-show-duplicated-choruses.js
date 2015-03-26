import React from 'react';
import classNames from 'classnames';

export default React.createClass({

    render: function () {

        const classString = classNames(
            'fa fa-folder fa-2x btn btn-default',
            {'active': this.props.showDuplicatedChoruses}
        );

        return (
            <li>
                <button
                    className={classString}
                    onClick={this.props.onToggleShowDuplicatedChoruses}
                />
            </li>
        );
    }

});

